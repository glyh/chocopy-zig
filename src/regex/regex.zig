const std = @import("std");
const Allocator = std.mem.Allocator;

const stdout = std.io.getStdOut().writer();
const assert = std.debug.assert;

fn last(slice: anytype) callconv(.Inline) std.meta.Child(@TypeOf(slice)) {
    return slice[slice.len - 1];
}

// Memory management

// Basic data structure
pub fn LinkNode(comptime T: type) type {
    return struct {
        const Self = @This();

        val: T = undefined,
        next: ?*Self = null,
        allocator: *Allocator,

        pub fn init(allocator: *Allocator) !*Self {
            var self = try allocator.create(Self);
            self.*.allocator = allocator;
            return self;
        }

        pub fn initWithVal(allocator: *Allocator, val: T) !*Self {
            var self = try allocator.create(Self);
            self.*.allocator = allocator;
            self.*.val = val;
            return self;
        }

        pub fn prepend(self: *Self, v: T) !*Self {
            const ret = try Self.initWithVal(self.*.allocator, v);
            ret.*.next = self;

            return ret;
        }
    };
}

pub fn Pair(comptime T1: type, comptime T2: type) type {
    return struct {
        const Self = @This();

        first: T1 = undefined,
        second: T2 = undefined,
        allocator: *Allocator,

        pub fn init(allocator: *Allocator) !*Self {
            var self = try allocator.create(Self);
            return self;
        }

        pub fn initWithVal(allocator: *Allocator, val1: T1, val2: T2) !*Self {
            var self = try allocator.create(Self);
            self.*.first = val1;
            self.*.second = val2;
            return self;
        }
    };
}

const RegexOperator = enum {
    Union, // '|'
    Concatenation,
    KleeneClosure, // '*'
    PositiveClosure, // '+'
    Wildcard, // '.'
    Optional, // '?'
};
const RegexElementTag = enum {
    Literal,
    Operator,
};
const RegexElement = union(RegexElementTag) {
    Literal: u8,
    Operator: RegexOperator,
};

// Regex building
const NodeMeta = struct {
    const Self = @This();

    element: RegexElement = undefined,
    size: usize = 0,
    nullable: bool = false,
    position: ?u32 = null,
    childs: ?*LinkNode(*Self) = null,
    father: ?*Self = null,

    first_position: std.ArrayList(Self) = undefined,
    last_position: std.ArrayList(Self) = undefined,

    pub fn init(allocator: *Allocator) !*Self {
        var self = try allocator.create(Self);
        self.*.childs = try LinkNode(*Self).init(allocator);
        return self;
    }
    pub fn initWithVal(allocator: *Allocator, element: RegexElement, position: ?u32) !*Self {
        var self = try allocator.create(Self);
        self.* = NodeMeta{ .element = element, .position = position };
        self.*.childs = try LinkNode(*Self).init(allocator);
        return self;
    }
};

fn prettyPrintTree(root: *NodeMeta, allocator: *Allocator) !void {
    // In zig, recursion call can't auto generate error sets, so I choose iteration.
    var stack = std.ArrayList(*Pair(*NodeMeta, u8)).init(allocator);
    const root_tagged = try Pair(*NodeMeta, u8).initWithVal(allocator, root, 0);
    try stack.append(root_tagged);
    while (true) {
        const top = stack.pop();
        const current = top.*.first;
        const indent = top.*.second;
        var i: u8 = 0;
        while (i < indent) : (i += 1) {
            try stdout.print("  ", .{});
        }
        try stdout.print("{} at {}\n", .{ current.*.element, current.*.position });
        var cur_child = current.*.childs;
        while (cur_child != null) : (cur_child = cur_child.?.*.next) {
            const child = try Pair(*NodeMeta, u8).initWithVal(allocator, cur_child.?.*.val, indent + 1);
            try stack.append(child);
        }
    }
}

fn link(father: *NodeMeta, son: *NodeMeta) !void {
    father.*.childs = try father.*.childs.?.*.prepend(son);
    son.*.father = father;
}

fn unlink(son: *NodeMeta) void {
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
            son.*.father = null;
            return;
        }
    }
    unreachable;
}

fn insertToFather(
    chain: *std.ArrayList(*NodeMeta),
    new_father: *NodeMeta,
) !void {
    const current = chain.pop();
    try stdout.print("Insert into the father of: {}", .{current.*.element});
    const grandfather = current.*.father.?;
    try chain.*.append(new_father);
    try chain.*.append(current);

    unlink(current);
    try link(grandfather, new_father);
    try link(new_father, current);
}

fn insertReduce(
    chain: *std.ArrayList(*NodeMeta),
    chain_parenthesis: *std.ArrayList(*NodeMeta),
    new: *NodeMeta,
    reduce_op: RegexOperator,
    allocator: *Allocator,
) !void {
    const current = last(chain.items);
    const current_parenthesis = last(chain_parenthesis.items);
    if (current == current_parenthesis) { //Comparing memory address
        try link(current, new);
        try chain.*.append(new);
    } else {
        if (current.*.father.?.*.element.Operator == reduce_op) {
            try link(current.*.father.?, new);
        } else {
            const concat = try NodeMeta.initWithVal(allocator, RegexElement{ .Operator = reduce_op }, null);
            try insertToFather(chain, concat);
            concat.childs = try concat.childs.?.*.prepend(new);
        }
    }
}

fn parseRegexToTree(expression: []const u8, allocator: *Allocator) !*NodeMeta { //Only supports ASCII.
    // Track some key chains
    var chain = std.ArrayList(*NodeMeta).init(allocator);
    defer chain.deinit();

    var chain_parenthesis = std.ArrayList(*NodeMeta).init(allocator);
    defer chain_parenthesis.deinit();

    // Keep message of positions
    var pos: u32 = 0;

    // Track some key nodes
    var root = try NodeMeta.initWithVal(allocator, RegexElement{ .Operator = RegexOperator.Union }, null);

    var escaped = false;
    var in_charset = false;

    var last_in_charset: ?u8 = null;
    var on_range: bool = false;

    try chain.append(root);
    try chain_parenthesis.append(root);

    for (expression) |char| {
        try stdout.print("Reading char: {c}\n", .{char});
        if (escaped) {
            const literal = try NodeMeta.initWithVal(allocator, RegexElement{ .Literal = char }, pos);
            pos += 1;
            try insertReduce(
                &chain,
                &chain_parenthesis,
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
                    if (last_in_charset != null) {
                        on_range = true;
                    } else {
                        const literal = try NodeMeta.initWithVal(allocator, RegexElement{ .Literal = char }, pos);
                        pos += 1;
                        try insertReduce(
                            &chain,
                            &chain_parenthesis,
                            literal,
                            RegexOperator.Union,
                            allocator,
                        );
                        last_in_charset = '-';
                    }
                },
                else => {
                    if (on_range) {
                        var i = last_in_charset.? + 1;

                        on_range = false;
                        last_in_charset = undefined;

                        while (i <= char) : (i += 1) {
                            const literal = try NodeMeta.initWithVal(allocator, RegexElement{ .Literal = i }, pos);
                            pos += 1;
                            try insertReduce(
                                &chain,
                                &chain_parenthesis,
                                literal,
                                RegexOperator.Union,
                                allocator,
                            );
                        }
                    } else {
                        const literal = try NodeMeta.initWithVal(allocator, RegexElement{ .Literal = char }, pos);
                        pos += 1;
                        try insertReduce(
                            &chain,
                            &chain_parenthesis,
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
                    const new_layer = try NodeMeta.initWithVal(allocator, RegexElement{ .Operator = RegexOperator.Union }, null);

                    try link(last(chain.items), new_layer);

                    try chain.append(new_layer);
                    try chain_parenthesis.append(new_layer);
                },
                ')' => {
                    const parenthesis_to_exit = chain_parenthesis.pop();
                    while (chain.pop() != parenthesis_to_exit) {}
                    try chain.append(parenthesis_to_exit);
                },
                '|' => {
                    while (last(chain.items) != current_parenthesis)
                        _ = chain.pop();
                },
                '*' => {
                    assert(current != current_parenthesis);
                    const kleene = try NodeMeta.initWithVal(allocator, RegexElement{ .Operator = RegexOperator.KleeneClosure }, null);
                    try insertToFather(
                        &chain,
                        kleene,
                    );
                },
                '+' => {
                    assert(current != current_parenthesis);
                    const positive = try NodeMeta.initWithVal(allocator, RegexElement{ .Operator = RegexOperator.PositiveClosure }, null);
                    try insertToFather(
                        &chain,
                        positive,
                    );
                },
                '?' => {
                    assert(current != current_parenthesis);
                    const optional = try NodeMeta.initWithVal(allocator, RegexElement{ .Operator = RegexOperator.Optional }, null);
                    try insertToFather(
                        &chain,
                        optional,
                    );
                },
                '.' => {
                    const wildcard = try NodeMeta.initWithVal(allocator, RegexElement{ .Operator = RegexOperator.Wildcard }, pos);
                    pos += 1;
                    try insertReduce(
                        &chain,
                        &chain_parenthesis,
                        wildcard,
                        RegexOperator.Concatenation,
                        allocator,
                    );
                },
                else => {
                    const literal = try NodeMeta.initWithVal(allocator, RegexElement{ .Literal = char }, pos);
                    pos += 1;
                    try insertReduce(
                        &chain,
                        &chain_parenthesis,
                        literal,
                        RegexOperator.Concatenation,
                        allocator,
                    );
                },
            }
        }
    }
    assert(!escaped and !in_charset);
    return root;
}
test "Build up tree for regex" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = &gpa.allocator;
    defer _ = gpa.deinit();
    try stdout.print("\nStart building tree...\n", .{});
    const t = try parseRegexToTree("(a|b)*ab", allocator);
    try stdout.print("Tree built!\n", .{});
    try prettyPrintTree(t, allocator);
}
