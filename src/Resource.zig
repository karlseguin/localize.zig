const std = @import("std");
const Parser = @import("Parser.zig");
const Message = @import("Message.zig");

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const Resource = @This();

_arena: ArenaAllocator,

// the index of the key is the index of the _locales
// _keys: ["en-US", "fr-FR"]
// _locales: [Locale(for en-US), Locale(for fr-FR)]
// It's essentially a k=>v map, but more efficient since we expect
// relatively few keys.
_keys: []const []const u8,

_locales: []Locale,

pub fn init(allocator: Allocator, locale_names: []const []const u8) !Resource {
    var arena = ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    const aa = arena.allocator();

    var keys = try aa.alloc([]const u8, locale_names.len);
    var locales = try aa.alloc(Locale, locale_names.len);

    for (locale_names, 0..) |l, i| {
        keys[i] = try aa.dupe(u8, l);
        locales[i] = .{};
    }

    return .{
        ._arena = arena,
        ._keys = keys,
        ._locales = locales,
    };
}

pub fn deinit(self: *const Resource) void {
    self._arena.deinit();
}

pub fn parser(self: *Resource, locale_name: []const u8, opts: Parser.Opts) !LocaleParser {
    var arena = &self._arena;
    const index = self.getLocaleIndex(locale_name) orelse return error.UnknownLocale;
    return .{
        .allocator = arena.allocator(),
        ._lookup = &self._locales[index].lookup,
        ._parser = try Parser.init(arena.child_allocator, arena.allocator(), opts),
    };
}

pub fn write(self: *const Resource, writer: anytype, locale_name: []const u8, key: []const u8, args: anytype) !void {
    const locale = self.getLocale(locale_name) orelse return error.UnknownLocale;
    return locale.write(writer, key, args);
}

pub fn getLocale(self: *const Resource, locale_name: []const u8) ?*const Locale {
    const index = self.getLocaleIndex(locale_name) orelse return null;
    return &self._locales[index];
}

fn getLocaleIndex(self: *const Resource, locale_name: []const u8) ?usize {
    for (self._keys, 0..) |l, i| {
        if (std.mem.eql(u8, l, locale_name)) {
            return i;
        }
    }
    return null;
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
