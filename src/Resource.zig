const std = @import("std");
const Parser = @import("Parser.zig");
const Message = @import("Message.zig");

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

pub fn Resource(comptime E: type) type {
    verifyEnum(E);

    return struct {
        _arena: ArenaAllocator,

        _locales: []Locale,

        const Self = @This();

        pub fn init(allocator: Allocator) !Self {
            var arena = ArenaAllocator.init(allocator);
            errdefer arena.deinit();

            const aa = arena.allocator();

            const field_len = @typeInfo(E).@"enum".fields.len;
            const locales = try aa.alloc(Locale, field_len);
            for (0..field_len) |i| {
                locales[i] = .{};
            }

            return .{
                ._arena = arena,
                ._locales = locales,
            };
        }

        pub fn deinit(self: *const Self) void {
            self._arena.deinit();
        }

        pub fn parser(self: *Self, l: E, opts: Parser.Opts) !LocaleParser {
            var arena = &self._arena;
            var locale = &self._locales[@intFromEnum(l)];
            return .{
                .allocator = arena.allocator(),
                ._lookup = &locale.lookup,
                ._parser = try Parser.init(arena.child_allocator, arena.allocator(), opts),
            };
        }

        pub fn write(self: *const Self, writer: anytype, l: E, key: []const u8, args: anytype) !void {
            const locale = &self._locales[@intFromEnum(l)];
            return locale.write(writer, key, args);
        }

        pub fn getLocale(self: *const Self, l: E) *const Locale {
            return &self._locales[@intFromEnum(l)];
        }
    };
}

fn verifyEnum(comptime E: type) void {
    const ti = @typeInfo(E);
    if (ti != .@"enum") {
        @compileError("Resource(E) type (E) must be an enum");
    }

    for (ti.@"enum".fields, 0..) |f, i| {
        if (f.value != i) {
            @compileError(std.fmt.comptimePrint("{s}.{s} should have a value of {d}, got {d}", .{@typeName(E), f.name, i, f.value}));
        }
    }
}

// Wrapper around a StringHashMap to limit the API that's exposed.
pub const Locale = struct {
    lookup: std.StringHashMapUnmanaged(Message) = .{},

    pub fn get(self: *const Locale, key: []const u8) ?Message {
        return self.lookup.get(key);
    }

    pub fn write(self: *const Locale, writer: anytype, key: []const u8, args: anytype) !void {
        const m = self.lookup.get(key) orelse return error.UnknownKey;
        return m.format(writer, args);
    }
};

const LocaleParser = struct {
    allocator: Allocator,
    _parser: Parser,
    _lookup: *std.StringHashMapUnmanaged(Message),

    pub fn deinit(self: *LocaleParser) void {
        self._parser.deinit();
    }

    // Doesn't dupe the key. Caller can use parser.allocator to dupe it if needed
    pub fn add(self: *LocaleParser, key: []const u8, src: []const u8) !void {
        const message = try self._parser.parseMessage(src);
        return self._lookup.put(self.allocator, key, message);
    }
};
