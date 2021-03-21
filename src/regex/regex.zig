const std = @import("std");
const Allocator = std.mem.Allocator;

const stdout = std.io.getStdOut().writer();
const assert = std.debug.assert;

fn last(slice: anytype) callconv(.Inline) std.meta.Child(@TypeOf(slice)) {
    return slice[slice.len - 1];
}

// Generic types
pub fn LinkNode(comptime T: type) type {
    return struct {
        const Self = @This();

        allocator: *Allocator,
        val: T,
        next: ?*Self,

        pub fn create(allocator: *Allocator, val: T) !*Self {
            var self = try allocator.create(Self); // Allocator give you nothing but undefined!
            self.* = Self{ .allocator = allocator, .val = val, .next = null };
            return self;
        }

        pub fn prepend(self: *Self, v: T) !*Self {
            assert(self.*.allocator != undefined);
            const ret = try Self.create(self.*.allocator, v);
            ret.*.next = self;
            return ret;
        }

        pub fn deinit(self: *Self) void {
            const allocator = self.*.allocator;
            allocator.*.destroy(self);
        }
    };
}

pub fn Pair(comptime T1: type, comptime T2: type) type {
    return struct {
        const Self = @This();

        allocator: *Allocator,
        first: T1,
        second: T2,

        pub fn create(allocator: *Allocator, val1: T1, val2: T2) !*Self {
            const self = try allocator.*.create(Self);
            self.* = Self{ .allocator = allocator, .first = val1, .second = val2 };
            return self;
        }

        pub fn deinit(self: *Self) void {
            self.*.allocator.*.destroy(self);
        }
    };
}

test "Pair" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = &gpa.allocator;

    var pair = try Pair(i32, i32).create(allocator, 1, 1);
    defer pair.deinit();
}

// Regex building
const RegexOperator = enum {
    Union, // '|'
    //UnionNot, // For charsets starts with ^
    Concatenation,
    KleeneClosure, // '*'
    PositiveClosure, // '+'
    Wildcard, // '.'
    Optional, // '?'
};

fn regexOperatorPriority(op: RegexOperator) callconv(.Inline) !u8 {
    return switch (op) {
        .Union => 0,
        .Concatenation => 1,
        else => unreachable,
    };
}

const RegexElementTag = enum {
    Literal,
    Operator,
};

const RegexElement = union(RegexElementTag) {
    Literal: u8,
    Operator: RegexOperator,
};

const NodeMeta = struct {
    const Self = @This();

    element: RegexElement,
    position: ?u32,

    size: usize,
    nullable: bool,
    childs: ?*LinkNode(*Self),
    father: ?*Self,

    first_position: *std.ArrayList(u32),
    last_position: *std.ArrayList(u32),

    allocator: *Allocator,

    pub fn create(allocator: *Allocator, element: RegexElement, position: ?u32) !*Self {
        var self = try allocator.create(Self);
        self.* = NodeMeta{
            .element = element,
            .position = position,

            .size = 0,
            .nullable = false,
            .childs = null,
            .father = null,

            .first_position = undefined,
            .last_position = undefined,

            .allocator = allocator,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.*.allocator.destroy(self);
    }
};

fn prettyPrintTree(root: *NodeMeta, allocator: *Allocator) !void {
    // In zig, recursion call can't auto generate error sets, so I choose iteration.
    var stack = std.ArrayList(*Pair(*NodeMeta, u8)).init(allocator);
    defer stack.deinit();
    const root_tagged = try Pair(*NodeMeta, u8).create(allocator, root, 0);
    try stack.append(root_tagged);
    while (stack.items.len != 0) {
        const top = stack.pop();
        defer top.*.deinit();
        const current = top.*.first;
        const indent = top.*.second;
        var i: u8 = 0;
        while (i < indent) : (i += 1) {
            try stdout.print("  ", .{});
        }
        try stdout.print("{*}:{} at {}\n", .{ current, current.*.element, current.*.position });
        var cur_child = current.*.childs;
        while (cur_child != null) : (cur_child = cur_child.?.*.next) {
            assert(cur_child.?.*.val != undefined);
            const child = try Pair(*NodeMeta, u8).create(allocator, cur_child.?.*.val, indent + 1);
            try stack.append(child);
        }
    }
}

fn deinitTree(root: *NodeMeta, allocator: *Allocator) !void {
    // In zig, recursion call can't auto generate error sets, so I choose iteration.
    var stack = std.ArrayList(*NodeMeta).init(allocator);
    defer stack.deinit();
    try stack.append(root);
    while (stack.items.len != 0) {
        const current = stack.pop();
        defer current.*.deinit();
        var cur_child = current.*.childs;
        while (cur_child != null) : ({
            const las = cur_child.?;
            defer las.*.deinit();
            cur_child = las.*.next;
        }) {
            assert(cur_child.?.*.val != undefined);
            try stack.append(cur_child.?.*.val);
        }
    }
}

fn link(father: *NodeMeta, son: *NodeMeta) callconv(.Inline) !void {
    if (father.*.childs == null) {
        father.*.childs = try LinkNode(*NodeMeta).create(father.*.allocator, son);
    } else {
        father.*.childs = try father.*.childs.?.*.prepend(son);
    }
    son.*.father = father;
}

test "Link and Free" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = &gpa.allocator;
    var a = try NodeMeta.create(allocator, RegexElement{ .Literal = '1' }, null);
    defer a.*.deinit();
    var b = try NodeMeta.create(allocator, RegexElement{ .Literal = '1' }, null);
    defer b.*.deinit();
    try link(a, b);
    a.*.childs.?.*.deinit();
}

fn unlink(son: *NodeMeta) callconv(.Inline) !void {
    var i = son.*.father.?.*.childs;
    var last_i: ?*LinkNode(*NodeMeta) = null;
    while (i != null) : ({
        last_i = i;
        i = i.?.*.next;
    }) {
        if (i.?.*.val == son) { //Comparing memory address
            if (last_i == null) {
                son.*.father.?.*.childs = son.*.father.?.*.childs.?.*.next;
            } else {
                last_i.?.*.next = i.?.*.next;
            }
            i.?.*.deinit();
            son.*.father = null;
            return;
        }
    }
    unreachable;
}

fn unlinkAll(father: *NodeMeta) callconv(.Inline) void {
    var i = father.*.childs;
    var last_i: ?*LinkNode(*NodeMeta) = null;
    while (i != null) : ({
        last_i = i;
        i = i.?.*.next;
    }) {
        if (last_i != null) {
            last_i.?.*.deinit();
        }
        const son = i.?.*.val;
        son.*.father = null;
    }
    if (last_i != null) {
        last_i.?.*.deinit();
    }
    father.*.childs = null;
}

fn insertToFatherSingle(
    chain: *std.ArrayList(*NodeMeta),
    last_node: *?*NodeMeta,
    new_father: *NodeMeta,
    allocator: *Allocator,
) callconv(.Inline) !void {
    assert(last_node.* != null);

    const old_father = last(chain.*.items);

    try unlink(last_node.*.?);
    try link(old_father, new_father);
    try link(new_father, last_node.*.?);

    last_node.* = new_father;
}

fn insertToFather(
    chain: *std.ArrayList(*NodeMeta),
    last_node: *?*NodeMeta,
    new_father: *NodeMeta,
    allocator: *Allocator,
) callconv(.Inline) !void {
    assert(last_node.* != null);

    const old_father = last(chain.*.items);
    var i = old_father.*.childs;
    var stack = std.ArrayList(*NodeMeta).init(allocator);
    defer stack.deinit();
    while (i != null) : ({
        i = i.?.*.next;
    }) {
        try stack.append(i.?.*.val);
    }
    unlinkAll(old_father);
    try link(old_father, new_father);
    for (stack.items) |j| {
        try link(new_father, j);
    }

    last_node.* = new_father;
}

fn insertBinaryOperator(
    chain: *std.ArrayList(*NodeMeta),
    chain_parenthesis: *std.ArrayList(*NodeMeta),
    last_node: *?*NodeMeta,
    new: *NodeMeta,
    reduce_op: RegexOperator,
    allocator: *Allocator,
) callconv(.Inline) !void {
    const current_parenthesis = last(chain_parenthesis.items);
    const priority_insert = try regexOperatorPriority(reduce_op);
    var current = last(chain.*.items);
    var priority_current: u8 = undefined;
    while (chain.*.items.len > 1) : ({
        last_node.* = chain.*.pop();
        current = last(chain.*.items);
    }) {
        priority_current = try regexOperatorPriority(current.*.element.Operator);
        if (priority_insert >= priority_current) {
            break;
        }
    }
    if (last_node.* == null) {
        assert(current.*.element.Operator == RegexOperator.Union);
        if (reduce_op == RegexOperator.Union) {
            try link(current, new);
        } else {
            const concat = try NodeMeta.create(allocator, RegexElement{ .Operator = reduce_op }, null);
            try link(current, concat);
            try link(concat, new);
            try chain.*.append(concat);
        }
    } else {
        if (priority_insert == priority_current) {
            try link(current, new);
        } else {
            const concat = try NodeMeta.create(allocator, RegexElement{ .Operator = reduce_op }, null);
            try insertToFather(chain, last_node, concat, allocator);
            try link(concat, new);
        }
    }
    last_node.* = new;
}

fn insertUnaryOperator(
    chain: *std.ArrayList(*NodeMeta),
    op: RegexOperator,
    last_node: *?*NodeMeta,
    allocator: *Allocator,
) callconv(.Inline) !void {
    assert(last_node.* != null);
    const op_element = try NodeMeta.create(allocator, RegexElement{ .Operator = op }, null);

    try insertToFatherSingle(chain, last_node, op_element, allocator);
}

fn readCharsetLiteral(
    last_node: *?*NodeMeta,
    last_in_charset: *?u8,
    on_range_charset: *bool,
    char: u8,
    sigma: []bool,
) callconv(.Inline) !void {
    if (on_range_charset.*) {
        var i = last_in_charset.*.? + 1;

        on_range_charset.* = false;
        last_in_charset.* = null;

        assert(i < char);
        while (i <= char) : (i += 1) {
            sigma[i] = true;
        }
    } else {
        sigma[char] = true;
        last_in_charset.* = char;
    }
}

const parseRegexToTreeResult = struct {
    root: *NodeMeta,
    pos: u32,
};
fn parseRegexToTree(expression: []const u8, allocator: *Allocator) !parseRegexToTreeResult {
    try stdout.print("Building tree for regex: \"{s}\"...\n", .{expression});
    // Only supports ASCII. Since I use Link List to store childs, they are inserted in the reversed order.
    // Track some key chains
    var chain = std.ArrayList(*NodeMeta).init(allocator);
    defer chain.deinit();

    var chain_parenthesis = std.ArrayList(*NodeMeta).init(allocator);
    defer chain_parenthesis.deinit();

    // Keep message of positions
    var pos: u32 = 0;

    // Track some key nodes
    var root = try NodeMeta.create(allocator, RegexElement{ .Operator = RegexOperator.Union }, null);
    var last_node: ?*NodeMeta = null;

    var escaped = false;

    var sigma = try allocator.alloc(bool, 1 << @sizeOf(u8));
    defer allocator.free(sigma);

    var in_charset = false;
    var last_in_charset: ?u8 = null;
    var on_range_charset: bool = false;
    var head_charset = true;
    var negatived_charset = false;

    try chain.append(root);
    try chain_parenthesis.append(root);

    for (expression) |char| {
        //try stdout.print("Reading char: {c}\n", .{char});
        if (escaped) {
            if (in_charset) {
                try readCharsetLiteral(
                    &last_node,
                    &last_in_charset,
                    &on_range_charset,
                    char,
                    sigma,
                );
            } else {
                const literal = try NodeMeta.create(allocator, RegexElement{ .Literal = char }, pos);
                pos += 1;
                try insertBinaryOperator(
                    &chain,
                    &chain_parenthesis,
                    &last_node,
                    literal,
                    if (in_charset) RegexOperator.Union else RegexOperator.Concatenation,
                    allocator,
                );
            }
            escaped = false;
        } else if (in_charset) {
            switch (char) {
                '^' => {
                    if (head_charset) {
                        negatived_charset = true;
                    } else {
                        try readCharsetLiteral(
                            &last_node,
                            &last_in_charset,
                            &on_range_charset,
                            char,
                            sigma,
                        );
                    }
                },
                '\\' => {
                    escaped = true;
                },
                '-' => {
                    if (last_in_charset != null and !on_range_charset) {
                        on_range_charset = true;
                    } else {
                        try readCharsetLiteral(
                            &last_node,
                            &last_in_charset,
                            &on_range_charset,
                            char,
                            sigma,
                        );
                    }
                },
                ']' => {
                    if (on_range_charset) {
                        sigma['-'] = true;
                        last_in_charset = '-';
                    }

                    const parenthesis_to_exit = chain_parenthesis.pop();
                    for (sigma) |val, index| {
                        if (val != negatived_charset) {
                            const literal = try NodeMeta.create(allocator, RegexElement{ .Literal = @truncate(u8, index) }, pos);
                            pos += 1;
                            try insertBinaryOperator(
                                &chain,
                                &chain_parenthesis,
                                &last_node,
                                literal,
                                RegexOperator.Union,
                                allocator,
                            );
                        }
                    }
                    while (chain.pop() != parenthesis_to_exit) {}
                    last_node = parenthesis_to_exit;

                    in_charset = false;
                },
                else => {
                    try readCharsetLiteral(
                        &last_node,
                        &last_in_charset,
                        &on_range_charset,
                        char,
                        sigma,
                    );
                },
            }
            head_charset = false;
        } else {
            const current = last(chain.items);
            const current_parenthesis = last(chain_parenthesis.items);
            switch (char) {
                '\\' => {
                    escaped = true;
                },
                '(' => {
                    const new_layer = try NodeMeta.create(allocator, RegexElement{ .Operator = RegexOperator.Union }, null);

                    try insertBinaryOperator(
                        &chain,
                        &chain_parenthesis,
                        &last_node,
                        new_layer,
                        RegexOperator.Concatenation,
                        allocator,
                    );
                    last_node = null;

                    //try link(last(chain.items), new_layer);
                    try chain.append(new_layer);
                    try chain_parenthesis.append(new_layer);
                },
                ')' => {
                    //unreachable; //FIXME
                    const parenthesis_to_exit = chain_parenthesis.pop();
                    while (chain.pop() != parenthesis_to_exit) {}
                    last_node = parenthesis_to_exit;
                    //try chain.append(parenthesis_to_exit);
                },
                '|' => {
                    while (last(chain.items) != current_parenthesis)
                        _ = chain.pop();
                    last_node = null;
                },
                '*' => {
                    try insertUnaryOperator(&chain, RegexOperator.KleeneClosure, &last_node, allocator);
                },
                '+' => {
                    try insertUnaryOperator(&chain, RegexOperator.PositiveClosure, &last_node, allocator);
                },
                '?' => {
                    try insertUnaryOperator(&chain, RegexOperator.Optional, &last_node, allocator);
                },
                '.' => {
                    const wildcard = try NodeMeta.create(allocator, RegexElement{ .Operator = RegexOperator.Wildcard }, pos);
                    pos += 1;
                    try insertBinaryOperator(
                        &chain,
                        &chain_parenthesis,
                        &last_node,
                        wildcard,
                        RegexOperator.Concatenation,
                        allocator,
                    );
                    last_node = wildcard;
                },
                '[' => {
                    const new_layer = try NodeMeta.create(allocator, RegexElement{ .Operator = RegexOperator.Union }, null);

                    try insertBinaryOperator(
                        &chain,
                        &chain_parenthesis,
                        &last_node,
                        new_layer,
                        RegexOperator.Concatenation,
                        allocator,
                    );
                    last_node = null;

                    //try link(last(chain.items), new_layer);
                    try chain.append(new_layer);
                    try chain_parenthesis.append(new_layer);

                    in_charset = true;
                    last_in_charset = null;
                    on_range_charset = false;
                    head_charset = true;
                    negatived_charset = false;
                    for (sigma) |_, index| {
                        sigma[index] = false;
                    }
                },
                //TODO: '^', '$', '(?:)'
                else => {
                    const literal = try NodeMeta.create(allocator, RegexElement{ .Literal = char }, pos);
                    pos += 1;
                    try insertBinaryOperator(
                        &chain,
                        &chain_parenthesis,
                        &last_node,
                        literal,
                        RegexOperator.Concatenation,
                        allocator,
                    );
                },
            }
        }
    }
    assert(!escaped and !in_charset);
    try stdout.print("Tree built!\n", .{});
    return parseRegexToTreeResult{ .root = root, .pos = pos };
}

fn mergePositions(out: **std.ArrayList(u32), in: *std.ArrayList(u32), allocator: *Allocator) !void {
    var ret = &(std.ArrayList(u32).init(allocator));
    var i: usize = 0;
    var j: usize = 0;
    while (i < out.*.items.len and j < in.items.len) {
        if (out.*.items[i] < in.items[j]) {
            try ret.append(out.*.items[i]);
            i += 1;
        } else if (out.*.items[i] > in.items[j]) {
            try ret.append(in.items[j]);
            j += 1;
        } else {
            try ret.append(in.items[j]);
            i += 1;
            j += 1;
        }
    }
    out.*.deinit();
    out.* = ret;
}

const NextMeta = struct {
    const Self = @This();

    literal: u32,

    next_position: ?*std.ArrayList(u32),

    allocator: *Allocator,

    pub fn create(allocator: *Allocator, literal: u32, position: ?u32) !*Self {
        var self = try allocator.create(Self);
        self.* = NextMeta{
            .literal = literal,

            .next_position = null,

            .allocator = allocator,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.*.allocator.destroy(self);
    }
};

fn linkNextSingle(pool: []NextMeta, from: u32, to: u32) callconv(.Inline) !void {
    if (pool[from].next_position == null) {
        pool[from].next_position = &(try std.ArrayList(u32).create(pool[from].allocator));
    } else {
        pool[from].next_position = try pool[from].next_position.prepend(to);
    }
}

fn linkNextMultiple(pool: []NextMeta, from: *std.ArrayList(u32), to: *std.ArrayList(u32)) callconv(.Inline) !void {
    for (from.items) |from_i| {
        for (to.items) |to_i| {
            try linkNextSingle(pool, from_i, to_i);
        }
    }
}

fn dpTree(root: *NodeMeta, pos: u32, allocator: *Allocator) !void {
    // In zig, recursion call can't auto generate error sets, so I choose iteration.
    var queue = std.ArrayList(*NodeMeta).init(allocator);
    defer queue.deinit();
    try queue.append(root);
    var head: u32 = 0;
    var stack = std.ArrayList(*NodeMeta).init(allocator);
    defer stack.deinit();

    while (queue.items.len != head) : (head += 1) {
        const cur = queue.items[head];
        var cur_child = cur.*.childs;
        while (cur_child != null) : (cur_child = cur_child.?.*.next) {
            assert(cur_child.?.*.val != undefined);
            try queue.append(cur_child.?.val);
        }
    }

    var rhead: usize = queue.items.len - 1;
    var pool = try allocator.alloc(NextMeta, pos + 1);
    //linkNextMultiple
    while (rhead >= 0) : (rhead -= 1) {
        const cur = queue.items[rhead];
        switch (cur.element) {
            .Operator => {
                switch (cur.element.Operator) {
                    .Union => {
                        cur.first_position = &(std.ArrayList(u32).init(allocator));
                        cur.last_position = &(std.ArrayList(u32).init(allocator));
                        cur.nullable = false;
                        var i = cur.*.childs;
                        while (i != null) : (i = i.?.*.next) {
                            try mergePositions(&cur.first_position, i.?.val.first_position, allocator);
                            try mergePositions(&cur.last_position, i.?.val.last_position, allocator);
                            cur.nullable = cur.nullable or i.?.val.nullable;
                        }
                    },
                    .Concatenation => {
                        //chain is reversed!
                        var nullable_prefix = true;
                        var nullable_suffix = true;
                        var i = cur.childs;
                        var las: ?*LinkNode(u32) = null;
                        while (i != null) : ({
                            las = i;
                            i = i.?.next;
                        }) {
                            if (las != null) {
                                linkNextMultiple(pool, i.?.val.last_position, las.?.val.first_position);
                            }
                            if (nullable_suffix) {
                                try mergePositions(&cur.*.last_position, i.?.val.last_position, allocator);
                                nullable_suffix = nullable_suffix and i.?.val.nullable;
                            }
                            stack.append(i.?);
                        }

                        while (stack.items.len != 0) {
                            var child = stack.pop();
                            if (nullable_prefix) {
                                try mergePositions(&cur.*.first_position, cur_child.*.first_position, allocator);
                                nullable_prefix = nullable_prefix and cur.*.nullable;
                            }
                        }
                        cur.*.nullable = nullable_prefix;
                    },
                    .KleeneClosure => {
                        var i_null = cur.childs;
                        while (i_null) |i| : (i_null = i.next) {}
                        unreachable;
                        //cur.*
                    },
                    .PositiveClosure => {
                        unreachable;
                    },
                    .Optional => {
                        unreachable;
                    },
                    .Wildcard => {
                        cur.*.first_position = std.ArrayList(u32).init(allocator);
                        cur.*.first_position.*.append(cur.*.position);
                        cur.*.last_position = std.ArrayList(u32).init(allocator);
                        cur.*.last_position.*.append(cur.*.position);
                        cur.nullable = false;
                    },
                }
            },
            .Literal => {
                cur.*.first_position = std.ArrayList(u32).init(allocator);
                cur.*.first_position.*.append(cur.*.position);
                cur.*.last_position = std.ArrayList(u32).init(allocator);
                cur.*.last_position.*.append(cur.*.position);
                cur.*.nullable = false;
            },
        }
    }
}

test "Build up tree for regex" {
    try stdout.print("\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = &gpa.allocator;
    defer _ = gpa.deinit();
    const ret = try parseRegexToTree("\\[.\\](a|b)+x([^0-9+-]yz(cd)?)+", allocator);
    defer {
        deinitTree(ret.root, allocator) catch |e| {
            stdout.print("Error happens when deiniting tree.", .{}) catch |e2| {};
        };
    }
    try dpTree(ret.root, ret.pos, allocator);
    try prettyPrintTree(ret.root, allocator);
}
