const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Parser = @import("Parser.zig");
pub const Message = @import("Message.zig");

pub fn parse(allocator: Allocator, src: []const u8, options: Parser.Options) !Message {
    var parser = try Parser.init(allocator, options);
    defer parser.deinit();
    return parser.parseMessage(src);
}

test Message {
    std.testing.refAllDecls(@This());
}
