const std = @import("std");

pub const resource = @import("resource.zig");

pub const Resource = resource.Resource;
pub const Locale = resource.Locale;

pub const Data = @import("Data.zig");
pub const Parser = @import("Parser.zig");
pub const Message = @import("Message.zig");

pub const EmptyLocale = &Locale{};

test Message {
    std.testing.refAllDecls(@This());
}
