const std = @import("std");

pub const allocator = std.testing.allocator;

pub const expectEqual = std.testing.expectEqual;
pub const expectError = std.testing.expectError;
pub const expectString = std.testing.expectEqualStrings;

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
var _arena: ?ArenaAllocator = null;

pub fn arena() Allocator {
    if (_arena == null) {
        _arena = ArenaAllocator.init(allocator);
    }
    return _arena.?.allocator();
}

pub fn reset() void {
    if (_arena) |a| {
        a.deinit();
        _arena = null;
    }
}
