const std = @import("std");

const Data = @import("Data.zig");

const Message = @This();

parts: []Part,

pub fn format(self: Message, writer: anytype, args: anytype) !void {
    const w: Writer(@TypeOf(writer), passThroughEncoder) = .{ .writer = writer };
    return self._format(w, args);
}

pub fn formatJson(self: Message, writer: anytype, args: anytype) !void {
    const stream = writer.stream;
    const w: Writer(@TypeOf(stream), jsonEncoder) = .{ .writer = stream };

    try writer.beginWriteRaw();
    try stream.writeByte('\"');
    try self._format(w, args);
    try stream.writeByte('\"');
    writer.endWriteRaw();
}

pub fn _format(self: Message, writer: anytype, args: anytype) !void {
    const T = @TypeOf(args);
    if (T == Data) {
        return DataArgs.renderParts(writer, args, self.parts);
    }

    const fields = @typeInfo(T).@"struct".fields;
    comptime var enum_fields: [fields.len]std.builtin.Type.EnumField = undefined;
    inline for (fields, 0..) |f, i| {
        enum_fields[i] = .{ .name = f.name, .value = i };
    }

    const Tag = @Type(.{ .@"enum" = .{
        .decls = &.{},
        .tag_type = u16,
        .fields = &enum_fields,
        .is_exhaustive = true,
    } });

    return StructArgs.renderParts(Tag, fields, writer, args, self.parts);
}

pub const Part = union(enum) {
    literal: []const u8,
    variable: Variable,
    plural: Plural,

    pub const Variable = struct {
        name: []const u8,
        constraint: Constraint = .none,

        pub const Constraint = enum {
            none,
            numeric,
        };
    };

    pub const Plural = struct {
        variable: Variable,
        zero: ?[]const Part = null,
        one: ?[]const Part = null,
        other: []const Part,
    };
};

// Used when the args passed to render is a struct / anonymous struct
const StructArgs = struct {
    fn renderParts(Tag: type, fields: []const std.builtin.Type.StructField, writer: anytype, args: anytype, parts: []const Part) !void {
        for (parts) |p| {
            try renderPart(Tag, fields, writer, args, p);
        }
    }

    fn renderPart(Tag: type, fields: []const std.builtin.Type.StructField, writer: anytype, args: anytype, part: Part) @TypeOf(writer).Error!void {
        switch (part) {
            .literal => |str| try writer.writeAll(str),
            .variable => |variable| {
                const variable_name = variable.name;
                const name_enum = std.meta.stringToEnum(Tag, variable_name);
                inline for (fields, 0..) |f, i| {
                    if (@as(Tag, @enumFromInt(i)) == name_enum) {
                        try writeValue(writer, @field(args, f.name), variable.constraint);
                        return;
                    }
                }
                return writePlaceholder(writer, variable_name);
            },
            .plural => |plural| {
                const variable_name = plural.variable.name;
                const name_enum = std.meta.stringToEnum(Tag, variable_name);
                inline for (fields, 0..) |f, i| {
                    if (@as(Tag, @enumFromInt(i)) == name_enum) {
                        const value = @field(args, f.name);
                        switch (getPluralCondition(f.type, value)) {
                            .zero => return renderParts(Tag, fields, writer, args, plural.zero orelse plural.other),
                            .one => return renderParts(Tag, fields, writer, args, plural.one orelse plural.other),
                            .other => return renderParts(Tag, fields, writer, args, plural.other),
                        }
                    }
                }
                return renderParts(Tag, fields, writer, args, plural.other);
            },
        }
    }

    fn writeValue(writer: anytype, value: anytype, constraint: Part.Variable.Constraint) !void {
        const T = @TypeOf(value);
        const type_info = @typeInfo(T);

        if (constraint == .numeric) {
            switch (type_info) {
                .int, .comptime_int, .float, .comptime_float, .optional => {},
                else => return writer.writeAll("NaN"),
            }
        }

        switch (type_info) {
            .int, .comptime_int => return std.fmt.formatInt(value, 10, .lower, .{}, writer),
            .float, .comptime_float => return std.fmt.format(writer, "{d}", .{value}),
            .optional => return if (value) |v| writeValue(writer, v, constraint) else writer.writeAll("null"),
            .pointer => |ptr| switch (ptr.size) {
                .Slice => return writeSlice(writer, @as([]const ptr.child, value)),
                .One => switch (@typeInfo(ptr.child)) {
                    .array => return writeSlice(writer, @as([]const std.meta.Elem(ptr.child), value)),
                    else => writeTypeError(T),
                },
                else => writeTypeError(T),
            },
            .array => return writeValue(writer, &value, constraint),
            else => return std.fmt.format(writer, "{any}", .{value}),
        }
    }

    fn writeSlice(writer: anytype, value: anytype) !void {
        const T = @TypeOf(value);
        if (T == []const u8) {
            return writer.writeAll(value);
        }
        writeTypeError(T);
    }

    fn writeTypeError(comptime T: type) void {
        @compileError("cannot render message with value of type " ++ @typeName(T));
    }

    fn getPluralCondition(comptime T: type, value: anytype) PluralRenderCondition {
        switch (@typeInfo(T)) {
            .comptime_int, .int => switch (value) {
                0 => return .zero,
                1 => return .one,
                else => return .other,
            },
            .comptime_float, .float => {
                if (value == 0.0) return .zero;
                if (value == 1.0) return .one;
                return .other;
            },
            .optional => return if (value) |v| getPluralCondition(@TypeOf(v), v) else return .other,
            else => return .other,
        }
    }
};

// Used when the args passed into render is a Data
// Short of converting a struct -> Data (which would be inefficient), I
// couldn't figure out how to dedupe this more!
const DataArgs = struct {
    fn renderParts(writer: anytype, args: Data, parts: []const Part) !void {
        for (parts) |p| {
            try renderPart(writer, args, p);
        }
    }

    fn renderPart(writer: anytype, args: Data, part: Part) @TypeOf(writer).Error!void {
        switch (part) {
            .literal => |str| try writer.writeAll(str),
            .variable => |variable| {
                const variable_name = variable.name;
                if (args.get(variable_name)) |value| {
                    return writeValue(writer, value, variable.constraint);
                }
                return writePlaceholder(writer, variable_name);
            },
            .plural => |plural| {
                const variable_name = plural.variable.name;
                if (args.get(variable_name)) |value| {
                    switch (getPluralCondition(value)) {
                        .zero => return renderParts(writer, args, plural.zero orelse plural.other),
                        .one => return renderParts(writer, args, plural.one orelse plural.other),
                        .other => return renderParts(writer, args, plural.other),
                    }
                }
                return renderParts(writer, args, plural.other);
            },
        }
    }

    fn getPluralCondition(value: Data.Value) PluralRenderCondition {
        switch (value) {
            .i64 => |v| switch (v) {
                0 => return .zero,
                1 => return .one,
                else => return .other,
            },
            .u64 => |v| switch (v) {
                0 => return .zero,
                1 => return .one,
                else => return .other,
            },
            .f64 => |v| {
                if (v == 0.0) return .zero;
                if (v == 1.0) return .one;
                return .other;
            },
            .f32 => |v| {
                if (v == 0.0) return .zero;
                if (v == 1.0) return .one;
                return .other;
            },
            else => return .other,
        }
    }

    fn writeValue(writer: anytype, value: Data.Value, constraint: Part.Variable.Constraint) !void {
        if (constraint == .numeric) {
            switch (value) {
                .i64, .u64, .f32, .f64 => {},
                else => return writer.writeAll("NaN"),
            }
        }
        return value.write(writer);
    }
};

fn writePlaceholder(writer: anytype, variable_name: []const u8) !void {
    try writer.writeAll("{");
    try writer.writeAll(variable_name);
    try writer.writeAll("}");
}

const PluralRenderCondition = enum {
    zero,
    one,
    other,
};

fn Writer(comptime W: type, comptime encoder: *const fn (writer: anytype, chars: []const u8) W.Error!void) type {
    return struct {
        writer: W,

        pub const Error = W.Error;

        const Self = @This();
        pub fn writeAll(self: Self, data: []const u8) !void {
            return encoder(self.writer, data);
        }

        pub fn writeBytesNTimes(self: Self, bytes: []const u8, n: usize) !void {
            for (0..n) |_| {
                try self.writeAll(bytes);
            }
        }
    };
}

fn passThroughEncoder(writer: anytype, data: []const u8) @TypeOf(writer).Error!void {
    return writer.writeAll(data);
}
fn jsonEncoder(writer: anytype, data: []const u8) @TypeOf(writer).Error!void {
    return std.json.encodeJsonStringChars(data, .{}, writer);
}

const t = @import("t.zig");
test "Message: struct simple" {
    defer t.reset();
    try testRender("hello", .{}, "hello");
    try testRender("{name}", .{}, "{name}");
    try testRender("{name}", .{ .name = "leto" }, "leto");
    try testRender("hello {name}", .{ .name = "ghanima" }, "hello ghanima");

    try testRender("over {power}!!", .{ .power = 9000 }, "over 9000!!");
    try testRender("over {power}!!", .{ .power = -1.23441 }, "over -1.23441!!");
    try testRender("over {power}? {answer}", .{ .power = @as(i64, 9001), .answer = true }, "over 9001? true");
    try testRender("under {power}? {answer}", .{ .power = @as(f32, -1.23441), .answer = @as(?bool, false) }, "under -1.23441? false");
    try testRender("under {power}? {answer}", .{ .power = 9000, .answer = null }, "under 9000? null");

    try testRender("{str}", .{ .str = "abc" }, "abc");
    try testRender("{str}", .{ .str = "123".* }, "123");
    {
        const str = [_]u8{ '1', 'b', '3' };
        try testRender("{str}", .{ .str = str }, "1b3");
    }

    try testRender("a: {a}; other: {other}", .{ .a = 1 }, "a: 1; other: {other}");
}

test "Message: struct plural" {
    defer t.reset();
    {
        const template =
            \\{count, plural,
            \\   =0 {zero cats}
            \\   =1 {one cat}
            \\   other {'#' of cats: #}
            \\ }
        ;
        try testRender(template, .{ .count = 0 }, "zero cats");
        try testRender(template, .{ .count = 1 }, "one cat");
        try testRender(template, .{ .count = 3 }, "# of cats: 3");
    }

    {
        const template =
            \\{count, plural,
            \\   other {# cats}
            \\ }
        ;
        try testRender(template, .{ .count = 0 }, "0 cats");
        try testRender(template, .{ .count = 1 }, "1 cats");
        try testRender(template, .{ .count = -4 }, "-4 cats");
    }

    {
        const template =
            \\{count, plural,
            \\   =0 {zero cats}
            \\   =1 {one cat}
            \\   other {'#' of cats: #}
            \\ }
        ;
        try testRender(template, .{ .count = 0.0 }, "zero cats");
        try testRender(template, .{ .count = 0.4 }, "# of cats: 0.4");
        try testRender(template, .{ .count = 1.0 }, "one cat");
        try testRender(template, .{ .count = 1.001 }, "# of cats: 1.001");
        try testRender(template, .{ .count = 3.3 }, "# of cats: 3.3");
    }

    {
        const template =
            \\{count, plural,
            \\   =0 {zero cats}
            \\   =1 {one cat}
            \\   other {'#' of cats: #}
            \\ }
        ;
        try testRender(template, .{}, "# of cats: {count}");
    }

    {
        const template =
            \\{count, plural,
            \\   =0 {zero cats}
            \\   =1 {one cat}
            \\   other {'#' of cats: #}
            \\ }
        ;
        try testRender(template, .{ .count = true }, "# of cats: NaN");
    }
}

test "Message: struct complex" {
    defer t.reset();
    const template =
        \\I have {cats, plural,
        \\   =0 {zero cats}
        \\   =1 {one cat}
        \\   other {# cats, but I feel that {perfect, plural,
        \\      zero {0}
        \\      one {one!}
        \\      other {roughly # is the right '#'}
        \\   }}
        \\ }. What about you?
    ;
    try testRender(template, .{ .cats = 0 }, "I have zero cats. What about you?");
    try testRender(template, .{ .cats = 1, .perfect = 2 }, "I have one cat. What about you?");
    try testRender(template, .{ .cats = 4, .perfect = 3 }, "I have 4 cats, but I feel that roughly 3 is the right #. What about you?");
    try testRender(template, .{ .cats = 5 }, "I have 5 cats, but I feel that roughly {perfect} is the right #. What about you?");
}

test "Message: data simple" {
    defer t.reset();
    const arena = t.arena();

    try testRender("hello", try Data.initFromStruct(arena, .{}), "hello");
    try testRender("{name}", try Data.initFromStruct(arena, .{}), "{name}");
    try testRender("{name}", try Data.initFromStruct(arena, .{ .name = "leto" }), "leto");
    try testRender("hello {name}", try Data.initFromStruct(arena, .{ .name = "ghanima" }), "hello ghanima");

    try testRender("over {power}!!", try Data.initFromStruct(arena, .{ .power = 9000 }), "over 9000!!");
    try testRender("over {power}!!", try Data.initFromStruct(arena, .{ .power = -1.23441 }), "over -1.23441!!");
    try testRender("over {power}? {answer}", try Data.initFromStruct(arena, .{ .power = @as(i64, 9001), .answer = true }), "over 9001? true");
    try testRender("under {power}? {answer}", try Data.initFromStruct(arena, .{ .power = @as(f32, -1.23441), .answer = @as(?bool, false) }), "under -1.23441? false");
    try testRender("under {power}? {answer}", try Data.initFromStruct(arena, .{ .power = 9000, .answer = null }), "under 9000? null");

    try testRender("{str}", try Data.initFromStruct(arena, .{ .str = "abc" }), "abc");
    try testRender("{str}", try Data.initFromStruct(arena, .{ .str = "123" }), "123");
    {
        const str = [_]u8{ '1', 'b', '3' };
        try testRender("{str}", try Data.initFromStruct(arena, .{ .str = &str }), "1b3");
    }

    try testRender("a: {a}; other: {other}", try Data.initFromStruct(arena, .{ .a = 1 }), "a: 1; other: {other}");
}

test "Message: data plural" {
    defer t.reset();
    const arena = t.arena();

    {
        const template =
            \\{count, plural,
            \\   =0 {zero cats}
            \\   =1 {one cat}
            \\   other {'#' of cats: #}
            \\ }
        ;
        try testRender(template, try Data.initFromStruct(arena, .{ .count = 0 }), "zero cats");
        try testRender(template, try Data.initFromStruct(arena, .{ .count = 1 }), "one cat");
        try testRender(template, try Data.initFromStruct(arena, .{ .count = 3 }), "# of cats: 3");
    }

    {
        const template =
            \\{count, plural,
            \\   other {# cats}
            \\ }
        ;
        try testRender(template, try Data.initFromStruct(arena, .{ .count = 0 }), "0 cats");
        try testRender(template, try Data.initFromStruct(arena, .{ .count = 1 }), "1 cats");
        try testRender(template, try Data.initFromStruct(arena, .{ .count = -4 }), "-4 cats");
    }

    {
        const template =
            \\{count, plural,
            \\   =0 {zero cats}
            \\   =1 {one cat}
            \\   other {'#' of cats: #}
            \\ }
        ;
        try testRender(template, try Data.initFromStruct(arena, .{ .count = 0.0 }), "zero cats");
        try testRender(template, try Data.initFromStruct(arena, .{ .count = 0.4 }), "# of cats: 0.4");
        try testRender(template, try Data.initFromStruct(arena, .{ .count = 1.0 }), "one cat");
        try testRender(template, try Data.initFromStruct(arena, .{ .count = 1.001 }), "# of cats: 1.001");
        try testRender(template, try Data.initFromStruct(arena, .{ .count = 3.3 }), "# of cats: 3.3");
    }

    {
        const template =
            \\{count, plural,
            \\   =0 {zero cats}
            \\   =1 {one cat}
            \\   other {'#' of cats: #}
            \\ }
        ;
        try testRender(template, try Data.initFromStruct(arena, .{}), "# of cats: {count}");
    }

    {
        const template =
            \\{count, plural,
            \\   =0 {zero cats}
            \\   =1 {one cat}
            \\   other {'#' of cats: #}
            \\ }
        ;
        try testRender(template, try Data.initFromStruct(arena, .{ .count = true }), "# of cats: NaN");
    }
}

test "Message: data complex" {
    defer t.reset();
    const arena = t.arena();

    const template =
        \\I have {cats, plural,
        \\   =0 {zero cats}
        \\   =1 {one cat}
        \\   other {# cats, but I feel that {perfect, plural,
        \\      zero {0}
        \\      one {one!}
        \\      other {roughly # is the right '#'}
        \\   }}
        \\ }. What about you?
    ;
    try testRender(template, try Data.initFromStruct(arena, .{ .cats = 0 }), "I have zero cats. What about you?");
    try testRender(template, try Data.initFromStruct(arena, .{ .cats = 1, .perfect = 2 }), "I have one cat. What about you?");
    try testRender(template, try Data.initFromStruct(arena, .{ .cats = 4, .perfect = 3 }), "I have 4 cats, but I feel that roughly 3 is the right #. What about you?");
    try testRender(template, try Data.initFromStruct(arena, .{ .cats = 5 }), "I have 5 cats, but I feel that roughly {perfect} is the right #. What about you?");
}

fn testRender(src: []const u8, args: anytype, expected: []const u8) !void {
    const Parser = @import("Parser.zig");

    var parser = try Parser.init(t.allocator, t.arena(), .{});
    defer parser.deinit();

    const msg = try parser.parseMessage(src);
    var arr = std.ArrayList(u8).init(t.allocator);
    defer arr.deinit();

    try msg.format(arr.writer(), args);
    try t.expectString(expected, arr.items);
}
