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
            return self;
        }

        pub fn initWithVal(allocator: *Allocator, val: T) !*Self {
            var self = try allocator.create(Self);
            self.*.val = val;
            return self;
        }

        pub fn prepend(self: *Self, v: T) !*Self {
            const ret = try self.*.allocator.create(Self);
            ret.*.val = v;
            ret.*.next = self;

            return ret;
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
    position: u32 = 0,
    childs: ?*LinkNode(*Self) = null,
    father: ?*Self = null,

    first_position: std.ArrayList(Self) = undefined,
    last_position: std.ArrayList(Self) = undefined,
};

fn prettyPrintTree(root: *NodeMeta, indent: i32) !void {
    var cur_child = root.*.childs;
    var i: usize = 0;
    while (i < indent) : (i += 1) {
        try stdout.print("  ", .{});
    }
    try stdout.print("{} at {}\n", .{ root.*.element, root.*.position });
    while (cur_child != null) : (cur_child = cur_child.next) {
        try prettyPrintTree(cur_child.?.*.val, indent + 1);
    }
}

fn link(father: *NodeMeta, son: *NodeMeta) !void {
    //current.*.*.childs = try current.*.*.childs.?.*.prepend(new);
    father.*.childs = try father.*.childs.?.*.prepend(son);
    son.*.father = father;
}

fn unlink(son: *NodeMeta) void {
    var i = son.*.father.*.childs;
    var last: ?*NodeMeta = null;
    while (i != null) : ({
        last = i;
        i = i.next;
    }) {
        if (i.val == &son) { //Comparing memory address
            last.*.next = i.*.next;
            son.*.father = null;
            return;
        }
    }
    unreachable;
}

fn insertToFather(chain: *std.ArrayList(*NodeMeta), current: **NodeMeta, new_father: *NodeMeta) void {
    assert(chain.pop() == current.*);
    try chain.append(new_father);

    const grandfather = current.father;
    unlink(current);
    link(grandfather, new_father);
    link(new_father, current);
    current.* = new_father;
}

fn insertReduce(
    chain: *std.ArrayList(*NodeMeta),
    current: **NodeMeta,
    current_parenthesis: **NodeMeta,
    new: *NodeMeta,
    reduce_op: RegexOperator,
    allocator: *Allocator,
) !void {
    if (current.* == current_parenthesis.*) { //Comparing memory address
        current.*.*.childs = try current.*.*.childs.?.*.prepend(new);
        current.* = new;
        try chain.*.append(new);
    } else {
        if (current.*.*.father.?.*.element.Operator == reduce_op) {
            link(current.*.*.father.?, new);
        } else {
            const concat = try allocator.create(NodeMeta);
            concat.* = NodeMeta{ .element = RegexElement{ .Operator = reduce_op } };
            insertToFather(chain, current, concat);
            concat.childs = try concat.childs.?.*.prepend(new);
        }
        current.* = new;
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
    var root = try allocator.create(NodeMeta); //try LinkNode(NodeMeta).init(allocator);
    root.* = NodeMeta{ .element = RegexElement{ .Operator = RegexOperator.Union }, .position = pos };
    pos += 1;

    var current_parenthesis = root;
    var current = root;

    var escaped = false;
    var in_charset = false;

    var last_in_charset: ?u8 = null;
    var on_range: bool = false;

    try chain.append(root);
    try chain_parenthesis.append(root);

    for (expression) |char| {
        if (escaped) {
            const literal = try allocator.create(NodeMeta);
            literal.* = NodeMeta{
                .element = RegexElement{ .Literal = char },
                .position = pos,
            };
            pos += 1;
            try insertReduce(
                &chain,
                &current,
                &current_parenthesis,
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
                        const literal = try allocator.create(NodeMeta);
                        literal.* = NodeMeta{
                            .element = RegexElement{ .Literal = char },
                            .position = pos,
                        };
                        pos += 1;
                        try insertReduce(&chain, &current, &current_parenthesis, literal, RegexOperator.Union, allocator);
                        last_in_charset = '-';
                    }
                },
                else => {
                    if (on_range) {
                        var i = last_in_charset.? + 1;

                        on_range = false;
                        last_in_charset = undefined;

                        while (i <= char) : (i += 1) {
                            const literal = try allocator.create(NodeMeta);
                            literal.* = NodeMeta{
                                .element = RegexElement{ .Literal = i },
                                .position = pos,
                            };
                            pos += 1;
                            try insertReduce(&chain, &current, &current_parenthesis, literal, RegexOperator.Union, allocator);
                        }
                    } else {
                        const literal = try allocator.create(NodeMeta);
                        literal.* = NodeMeta{
                            .element = RegexElement{ .Literal = char },
                            .position = pos,
                        };
                        pos += 1;
                        try insertReduce(&chain, &current, &current_parenthesis, literal, RegexOperator.Union, allocator);
                        last_in_charset = char;
                    }
                },
            }
        } else {
            switch (char) {
                '\\' => {
                    escaped = true;
                },
                '(' => {
                    const new_layer = try allocator.create(NodeMeta);
                    new_layer.* = NodeMeta{
                        .element = RegexElement{ .Operator = RegexOperator.Union },
                    };
                    _ = try current.childs.?.*.prepend(new_layer);

                    try chain.append(new_layer);
                    try chain_parenthesis.append(new_layer);

                    current = new_layer;
                    current_parenthesis = current;
                },
                ')' => {
                    const parenthesis_to_exit = chain_parenthesis.pop();
                    while (chain.pop() != parenthesis_to_exit) {}

                    current = last(chain.items);
                    current_parenthesis = current;
                },
                '|' => {
                    while (last(chain.items) != current_parenthesis)
                        _ = chain.pop();
                    current = current_parenthesis;
                },
                '*' => {
                    assert(current != current_parenthesis);
                    const kleene = try allocator.create(NodeMeta);
                    kleene.* = NodeMeta{
                        .element = RegexElement{ .Operator = RegexOperator.KleeneClosure },
                    };
                    insertToFather(&chain, &current, kleene);
                },
                '+' => {
                    assert(current != current_parenthesis);
                    const positive = try allocator.create(NodeMeta);
                    positive.* = NodeMeta{
                        .element = RegexElement{ .Operator = RegexOperator.PositiveClosure },
                    };
                    insertToFather(&chain, &current, positive);
                },
                '?' => {
                    assert(current != current_parenthesis);
                    const optional = try allocator.create(NodeMeta);
                    optional.* = NodeMeta{
                        .element = RegexElement{ .Operator = RegexOperator.Optional },
                    };
                    insertToFather(&chain, &current, optional);
                },
                '.' => {
                    const wildcard = try allocator.create(NodeMeta);
                    wildcard.* = NodeMeta{
                        .element = RegexElement{ .Operator = RegexOperator.Wildcard },
                        .position = pos,
                    };
                    pos += 1;
                    try insertReduce(&chain, &current, &current_parenthesis, wildcard, RegexOperator.Concatenation, allocator);
                },
                else => {
                    const literal = try allocator.create(NodeMeta);
                    literal.* = NodeMeta{
                        .element = RegexElement{ .Literal = char },
                        .position = pos,
                    };
                    pos += 1;
                    try insertReduce(&chain, &current, &current_parenthesis, literal, RegexOperator.Concatenation, allocator);
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
    const t = try parseRegexToTree("(a|b)*ab", allocator);
    try prettyPrintTree(t, 0);
}
