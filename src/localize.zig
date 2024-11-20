const std = @import("std");

pub const Data = @import("Data.zig");
pub const Parser = @import("Parser.zig");
pub const Message = @import("Message.zig");
pub const Resource = @import("Resource.zig");
pub const Locale = Resource.Locale;

pub const EmptyLocale = &Locale{};

test Message {
    std.testing.refAllDecls(@This());
}
