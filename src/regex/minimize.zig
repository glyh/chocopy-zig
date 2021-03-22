const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();
const Allocator = std.mem.Allocator;
const HashSet = std.AutoHashMap(u32, bool);

var buf: [256]u8 = undefined;

fn getLine() ![]u8 {
    if (try stdin.readUntilDelimiterOrEof(buf[0..], '\n')) |user_input| {
        return user_input;
    } else return "";
}

var it_split: ?std.mem.SplitIterator = null;
fn readUnsigned() !u32 {
    if (it_split == null) it_split = std.mem.split(try getLine(), " ");
    while (it_split.?.next()) |cur| {
        if (cur.len != 0) {
            return std.fmt.parseUnsigned(u32, cur, 10);
        }
    }
    it_split = std.mem.split(try getLine(), " ");
    return readUnsigned();
}

test "readUnsigned" {
    try stdout.print("\n", .{});
    var i = readUnsigned();
    var j = readUnsigned();
    try stdout.print("{} {}\n ", .{ i, j });
}

const Sigma = 2;
var transition: [][]u32 = undefined;
var transition_reverse: [][](*HashSet) = undefined;

fn alloc2d(comptime t: type, m: u32, n: u32, allocator: *Allocator) callconv(.Inline) ![][]t {
    const array = try allocator.alloc([]t, m);
    for (array) |_, index| {
        array[index] = try allocator.alloc(t, n);
    }
    return array;
}

fn free2d(comptime t: type, array: [][]t, allocator: *Allocator) callconv(.Inline) void {
    for (array) |_, index| {
        allocator.free(array[index]);
    }
    allocator.free(array);
}

test "Alloc 2D Array" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = &gpa.allocator;
    defer _ = gpa.deinit();

    var array = try alloc2d(*HashSet, 4, 4, allocator);
    defer free2d(*HashSet, array, allocator);

    for (array) |_, i| {
        for (array[i]) |_, j| {
            array[i][j] = try allocator.create(HashSet);
            array[i][j].* = HashSet.init(allocator);
        }
    }
    defer {
        for (array) |_, i| {
            for (array[i]) |_, j| {
                array[i][j].deinit();
                allocator.destroy(array[i][j]);
            }
        }
    }
}

fn link(from: u32, to: u32, edge: u8, allocator: *Allocator) callconv(.Inline) !void {
    transition[from][edge] = to;
    _ = try transition_reverse[to][edge].getOrPut(from);
}

fn mergeSet(out: *HashSet, in: *HashSet) !void {
    var it = in.iterator();
    while (it.next()) |kv| {
        _ = try out.getOrPut(kv.key);
    }
}

const SubsetMeta = struct {
    elements: *HashSet, reversed: [Sigma]*HashSet, id: u32
};

fn print_set(s: *HashSet) callconv(.Inline) !void {
    var it = s.iterator();
    try stdout.print("{{", .{});
    while (it.next()) |kv| {
        try stdout.print("{}, ", .{kv.key});
    }
    try stdout.print("}}", .{});
}
var L: []std.AutoHashMap(u32, *SubsetMeta) = undefined;

var biset_cnt: u32 = 0;

pub fn initBiSubset(Bi: *SubsetMeta, allocator: *Allocator) callconv(.Inline) !void {
    var a: u32 = 0;
    while (a < Sigma) : (a += 1) {
        var cur = Bi.elements;
        Bi.reversed[a] = try allocator.create(HashSet);
        Bi.reversed[a].* = HashSet.init(allocator);

        var it = cur.iterator();
        while (it.next()) |kv| {
            try mergeSet(Bi.reversed[a], transition_reverse[kv.key][a]);
        }
    }
    Bi.id = biset_cnt;
    biset_cnt += 1;
}

pub fn destroyBiSubset(Bi: *SubsetMeta, allocator: *Allocator) callconv(.Inline) void {
    Bi.elements.deinit();
    allocator.destroy(Bi.elements);
    var a: u32 = 0;
    while (a < Sigma) : (a += 1) {
        Bi.reversed[a].deinit();
        allocator.destroy(Bi.reversed[a]);
    }
    allocator.destroy(Bi);
}

test "Free test" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = &gpa.allocator;
    const a = try allocator.create(HashSet);
    a.* = HashSet.init(allocator);
    defer {
        a.deinit();
        allocator.destroy(a);
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = &gpa.allocator;
    defer _ = gpa.deinit();

    const n: u32 = try readUnsigned();

    transition = try alloc2d(u32, n, Sigma, allocator);
    defer free2d(u32, transition, allocator);

    transition_reverse = try alloc2d(*HashSet, n, Sigma, allocator);
    defer free2d(*HashSet, transition_reverse, allocator);
    for (transition_reverse) |_, state| {
        for (transition_reverse[state]) |_, alpha| {
            transition_reverse[state][alpha] = try allocator.create(HashSet);
            transition_reverse[state][alpha].* = HashSet.init(allocator);
        }
    }
    defer {
        for (transition_reverse) |state| {
            for (state) |edges| {
                edges.deinit();
                allocator.destroy(edges);
            }
        }
    }

    L = try allocator.alloc(std.AutoHashMap(u32, *SubsetMeta), Sigma);
    for (L) |_, alpha| {
        L[alpha] = std.AutoHashMap(u32, *SubsetMeta).init(allocator);
    }
    defer {
        for (L) |_, index| {
            L[index].deinit();
        }
        allocator.free(L);
    }
    // Initialize
    var i: u32 = 0;
    while (i < n) : (i += 1) {
        try link(i, try readUnsigned(), 0, allocator);
        try link(i, try readUnsigned(), 1, allocator);
    }

    const n_accepting: u32 = try readUnsigned();

    const accepting = try allocator.alloc(bool, n);
    defer {
        allocator.free(accepting);
    }

    i = 0;
    while (i < n) : (i += 1) {
        accepting[i] = false;
    }

    i = 0;
    while (i < n_accepting) : (i += 1) {
        accepting[try readUnsigned()] = true;
    }

    var queue = std.TailQueue(*SubsetMeta){};
    defer {
        while (queue.pop()) |node| {
            destroyBiSubset(node.data, allocator);
            allocator.destroy(node);
        }
    }

    var a: u8 = 0;
    {
        const set_accept = try allocator.create(SubsetMeta);
        set_accept.elements = try allocator.create(HashSet);
        set_accept.elements.* = HashSet.init(allocator);

        const set_others = try allocator.create(SubsetMeta);
        set_others.elements = try allocator.create(HashSet);
        set_others.elements.* = HashSet.init(allocator);

        for (accepting) |yes, index| {
            if (yes) {
                _ = try set_accept.elements.getOrPut(@truncate(u32, index));
            } else {
                _ = try set_others.elements.getOrPut(@truncate(u32, index));
            }
        }

        try initBiSubset(set_others, allocator);
        try initBiSubset(set_accept, allocator);

        var n_tmp = try allocator.create(@TypeOf(queue).Node);
        n_tmp.* = @TypeOf(queue).Node{ .data = set_others };
        queue.append(n_tmp);
        n_tmp = try allocator.create(@TypeOf(queue).Node);
        n_tmp.* = @TypeOf(queue).Node{ .data = set_accept };
        queue.append(n_tmp);

        while (a < Sigma) : (a += 1) {
            var L_a_primary: ?*SubsetMeta = null;
            var it = queue.first;
            while (it) |subset| : (it = subset.next) {
                if (L_a_primary == null or subset.data.reversed[a].count() < L_a_primary.?.reversed[a].count()) {
                    L_a_primary = subset.data;
                }
            }

            L[a] = std.AutoHashMap(u32, *SubsetMeta).init(allocator);
            try L[a].put(L_a_primary.?.id, L_a_primary.?);
        }
    }

    //var q_replace = std.TailQueue(*SubsetMeta){};
    var new_elements = try allocator.create(HashSet);
    new_elements.* = HashSet.init(allocator);
    defer {
        new_elements.deinit();
        allocator.destroy(new_elements);
    }
    while (true) {
        var L_i: ?*SubsetMeta = null;

        a = 0;
        var selected: ?u32 = null;
        outer: while (a < Sigma) : (a += 1) {
            var it = L[a].iterator();
            while (it.next()) |kv| {
                selected = kv.key;
                break :outer;
            }
        }

        if (selected) |key| {
            var cur = L[a].get(key).?;
            _ = L[a].remove(key);
            var it = queue.first;

            try stdout.print("Set selected for partition: ", .{});
            try print_set(cur.elements);
            try stdout.print("\n", .{});

            while (it) |subset| : (it = subset.next) { //There's some redundent work on the queue, will be fixed soon.
                var B_j = subset.data;
                var it_cur = cur.reversed[a].iterator();

                while (it_cur.next()) |kv| {
                    if (B_j.elements.get(kv.key) != null) {
                        try stdout.print("There exists {} in B_j, such that it can goes to current set\n", .{kv.key});
                        _ = try new_elements.getOrPut(kv.key);
                        //_ = B_j.elements.remove(kv.key);
                        //Do this here would cause bug, never change some data structure when iterating through it.
                    }
                }
                it_cur = new_elements.iterator();
                while (it_cur.next()) |kv| {
                    _ = B_j.elements.remove(kv.key);
                }

                if (B_j.elements.count() == 0) {
                    const tmp = B_j.elements;
                    B_j.elements = new_elements;
                    new_elements = tmp;
                } else if (new_elements.count() != 0) {
                    try stdout.print("Seperate into ", .{});
                    try print_set(B_j.elements);
                    try stdout.print(" and ", .{});
                    try print_set(new_elements);
                    try stdout.print("\n", .{});

                    const B_k = try allocator.create(SubsetMeta);
                    B_k.elements = new_elements;
                    new_elements = try allocator.create(HashSet);
                    new_elements.* = HashSet.init(allocator);
                    //Free first!
                    var alpha: u8 = 0;
                    while (alpha < Sigma) : (alpha += 1) {
                        B_j.reversed[alpha].deinit();
                        allocator.destroy(B_j.reversed[alpha]);
                    }
                    try initBiSubset(B_j, allocator);
                    try initBiSubset(B_k, allocator);
                    if (B_j.reversed[a].count() < B_k.reversed[a].count()) {
                        try L[a].put(B_j.id, B_j);
                    } else {
                        try L[a].put(B_k.id, B_k);
                    }
                    var node_tmp = try allocator.create(@TypeOf(queue).Node);
                    node_tmp.* = std.TailQueue(*SubsetMeta).Node{ .data = B_k };

                    queue.append(node_tmp);
                }
            }
            try stdout.print("\n", .{});
        } else {
            break;
        }
    }
    var it_out = queue.first;
    while (it_out) |node| : (it_out = node.next) {
        var elements = node.data.elements;
        try stdout.print("{{", .{});

        var mit = elements.iterator();
        while (mit.next()) |kv| {
            try stdout.print("{}, ", .{kv.key});
        }
        try stdout.print("}}\n", .{});
    }
}
