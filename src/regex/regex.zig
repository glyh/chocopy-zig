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
            //stdout.print("Alloc: {*}\n", .{self}) catch |e| {};
            return self;
        }

        pub fn prepend(self: *Self, v: T) !*Self {
            assert(self.*.allocator != undefined);
            const ret = try Self.create(self.*.allocator, v);
            ret.*.next = self;
            return ret;
        }

        pub fn deinit(self: *Self) void {
            //try stdout.print("Free {*}\n", .{self});
            const allocator = self.*.allocator;

            //stdout.print("LinkNode deiniting {*}\n", .{self}) catch |e| {};
            allocator.*.destroy(self);
            //var i: ?*Self = self;
            // var j: ?*Self = null;

            // while (i != null) : (i = j) {
            //     j = i.?.*.next;
            //     allocator.*.destroy(i.?);
            //     stdout.print("Destroy: {*}\n", .{self}) catch |e| {};
            // }
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

    first_position: std.ArrayList(Self),
    last_position: std.ArrayList(Self),

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
        //if (self.childs != null) {
        //    self.*.childs.?.*.deinit();
        //}
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
        //try stdout.print("Deiniting: {*}\n", .{current});
        defer current.*.deinit();
        var cur_child = current.*.childs;
        while (cur_child != null) : ({
            //try stdout.print("{*}=>{*}\n", .{ cur_child, cur_child.?.*.val });
            const las = cur_child.?;
            //try stdout.print("want to free {*}\n", .{las});
            defer las.*.deinit();
            cur_child = las.*.next;
        }) {
            assert(cur_child.?.*.val != undefined);
            try stack.append(cur_child.?.*.val);
        }
    }
}

fn link(father: *NodeMeta, son: *NodeMeta) callconv(.Inline) !void {
    //try stdout.print("Link {*}:{} -> {*}:{}\n", .{ father, father.*.element, son, son.*.element });
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

fn unlink_all(father: *NodeMeta) callconv(.Inline) void {
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
    unlink_all(old_father);
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
) !void {
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
            //try chain.*.append(last_node);
            try insertToFather(chain, last_node, concat, allocator);
            try link(concat, new);
            //_ = chain.pop();
            //try chain.append(concat);
        }
    }
    last_node.* = new;
}

fn insertUnaryOperator(chain: *std.ArrayList(*NodeMeta), op: RegexOperator, last_node: *?*NodeMeta, allocator: *Allocator) !void {

    // chain: *std.ArrayList(*NodeMeta),
    // last_node: *?*NodeMeta,
    // new_father: *NodeMeta,
    // allocator: *Allocator,
    //
    assert(last_node.* != null);
    const op_element = try NodeMeta.create(allocator, RegexElement{ .Operator = op }, null);
    //last_node.* = null;

    //try chain.*.append(last_node.*.?);
    try insertToFatherSingle(chain, last_node, op_element, allocator);
    //last_node.* = op_element;
}

fn parseRegexToTree(expression: []const u8, allocator: *Allocator) !*NodeMeta {
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
    var in_charset = false;

    var last_in_charset: ?u8 = null;
    var on_range: bool = false;

    try chain.append(root);
    try chain_parenthesis.append(root);

    for (expression) |char| {
        //try stdout.print("Reading char: {c}\n", .{char});
        if (escaped) {
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
            escaped = false;
        } else if (in_charset) {
            switch (char) {
                '\\' => {
                    escaped = true;
                },
                '-' => {
                    if (last_in_charset != null and !on_range) {
                        on_range = true;
                    } else {
                        const literal = try NodeMeta.create(allocator, RegexElement{ .Literal = char }, pos);
                        pos += 1;
                        try insertBinaryOperator(
                            &chain,
                            &chain_parenthesis,
                            &last_node,
                            literal,
                            RegexOperator.Union,
                            allocator,
                        );
                        last_in_charset = '-';
                    }
                },
                ']' => {
                    //assert(on_range == false);
                    if (on_range) {
                        const literal = try NodeMeta.create(allocator, RegexElement{ .Literal = '-' }, pos);
                        pos += 1;
                        try insertBinaryOperator(
                            &chain,
                            &chain_parenthesis,
                            &last_node,
                            literal,
                            RegexOperator.Union,
                            allocator,
                        );
                        last_in_charset = '-';
                    }

                    //unreachable; //FIXME
                    const parenthesis_to_exit = chain_parenthesis.pop();
                    while (chain.pop() != parenthesis_to_exit) {}
                    last_node = parenthesis_to_exit;
                    //try chain.append(parenthesis_to_exit);

                    in_charset = false;
                },
                else => {
                    if (on_range) {
                        var i = last_in_charset.? + 1;

                        on_range = false;
                        last_in_charset = undefined;

                        assert(i < char);
                        while (i <= char) : (i += 1) {
                            try stdout.print("In charset: {c}\n", .{i});
                            const literal = try NodeMeta.create(allocator, RegexElement{ .Literal = i }, pos);
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
                    } else {
                        try stdout.print("In charset: {c}\n", .{char});
                        const literal = try NodeMeta.create(allocator, RegexElement{ .Literal = char }, pos);
                        pos += 1;
                        try insertBinaryOperator(
                            &chain,
                            &chain_parenthesis,
                            &last_node,
                            literal,
                            RegexOperator.Union,
                            allocator,
                        );
                        last_in_charset = char;
                    }
                },
            }
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
                },
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
    return root;
}
test "Build up tree for regex" {
    try stdout.print("\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = &gpa.allocator;
    defer _ = gpa.deinit();
    //try stdout.print("\nStart building tree...\n", .{});
    const t = try parseRegexToTree("\\[\\](a|b)+x([0-9+-]yz(cd)?)+", allocator);
    //try dpTree(t);
    //const t = try parseRegexToTree("()*", allocator);
    //try stdout.print("Tree built!\n", .{});
    try prettyPrintTree(t, allocator);
    try deinitTree(t, allocator);
}
