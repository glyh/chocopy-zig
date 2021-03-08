const std = @import("std");
const Allocator = std.mem.Allocator;

const LinkNode = struct {
    val: i32,
    next: ?*LinkNode,
};
pub fn alloc(allocator: *Allocator) !*LinkNode {
    return try allocator.*.create(LinkNode);
}
test "Memory leak" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var i: i32 = 0;
    var p: ?*LinkNode = null;
    while (i < 10) : (i = i + 1) {
        var new = try alloc(&gpa.allocator);
        new.* = .{ .val = i, .next = p };
        p = new;
    }
    gpa.allocator.destroy(p.?);
}
