const std = @import("std");

parts: []Part,
arena: std.heap.ArenaAllocator,

const Message = @This();

pub fn deinit(self: Message) void {
    self.arena.deinit();
}

pub fn render(self: Message, writer: anytype, args: anytype) !void {
    const fields = @typeInfo(@TypeOf(args)).@"struct".fields;
    comptime var enum_fields: [fields.len]std.builtin.Type.EnumField = undefined;
    inline for (fields, 0..) |f, i| {
        enum_fields[i] = .{.name = f.name, .value = i};
    }

    const Tag = @Type(.{.@"enum" = .{
        .decls = &.{},
        .tag_type = u16,
        .fields = &enum_fields,
        .is_exhaustive = true,
    }});
    return renderParts(Tag, fields, writer, args, self.parts);
}

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
            try writer.writeByte('{');
            try writer.writeAll(variable_name);
            try writer.writeByte('}');
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

const PluralRenderCondition = enum {
    zero,
    one,
    other,
};

const t = @import("t.zig");
test "Message: simple" {
    try testRender("hello", .{}, "hello");
    try testRender("{name}", .{}, "{name}");
    try testRender("{name}", .{.name = "leto"}, "leto");
    try testRender("hello {name}", .{.name = "ghanima"}, "hello ghanima");

    try testRender("over {power}!!", .{.power = 9000}, "over 9000!!");
    try testRender("over {power}!!", .{.power = -1.23441}, "over -1.23441!!");
    try testRender("over {power}? {answer}", .{.power = @as(i64, 9001), .answer = true}, "over 9001? true");
    try testRender("under {power}? {answer}", .{.power = @as(f32, -1.23441), .answer = @as(?bool, false)}, "under -1.23441? false");
    try testRender("under {power}? {answer}", .{.power = 9000, .answer = null}, "under 9000? null");

    try testRender("{str}", .{.str = "abc"}, "abc");
    try testRender("{str}", .{.str = "123".*}, "123");
    {
        const str = [_]u8{'1', 'b', '3'};
        try testRender("{str}", .{.str = str}, "1b3");
    }

    try testRender("a: {a}; other: {other}", .{.a = 1}, "a: 1; other: {other}");
}

test "Message: plural" {
    {
        const template = \\{count, plural,
            \\   =0 {zero cats}
            \\   =1 {one cat}
            \\   other {'#' of cats: #}
            \\ }
        ;
        try testRender(template, .{.count = 0}, "zero cats");
        try testRender(template, .{.count = 1}, "one cat");
        try testRender(template, .{.count = 3}, "# of cats: 3");
    }

    {
        const template = \\{count, plural,
            \\   other {# cats}
            \\ }
        ;
        try testRender(template, .{.count = 0}, "0 cats");
        try testRender(template, .{.count = 1}, "1 cats");
        try testRender(template, .{.count = -4}, "-4 cats");
    }

    {
        const template = \\{count, plural,
            \\   =0 {zero cats}
            \\   =1 {one cat}
            \\   other {'#' of cats: #}
            \\ }
        ;
        try testRender(template, .{.count = 0.0}, "zero cats");
        try testRender(template, .{.count = 0.4}, "# of cats: 0.4");
        try testRender(template, .{.count = 1.0}, "one cat");
        try testRender(template, .{.count = 1.001}, "# of cats: 1.001");
        try testRender(template, .{.count = 3.3}, "# of cats: 3.3");
    }

    {
        const template = \\{count, plural,
            \\   =0 {zero cats}
            \\   =1 {one cat}
            \\   other {'#' of cats: #}
            \\ }
        ;
        try testRender(template, .{}, "# of cats: {count}");
    }

   {
       const template = \\{count, plural,
           \\   =0 {zero cats}
           \\   =1 {one cat}
           \\   other {'#' of cats: #}
           \\ }
       ;
       try testRender(template, .{.count = true}, "# of cats: NaN");
   }
}

test "Message: complex" {
    const template = \\I have {cats, plural,
        \\   =0 {zero cats}
        \\   =1 {one cat}
        \\   other {# cats, but I feel that {perfect, plural,
        \\      zero {0}
        \\      one {one!}
        \\      other {roughly # is the right '#'}
        \\   }}
        \\ }. What about you?
    ;
    try testRender(template, .{.cats = 0}, "I have zero cats. What about you?");
    try testRender(template, .{.cats = 1, .perfect = 2}, "I have one cat. What about you?");
    try testRender(template, .{.cats = 4, .perfect = 3}, "I have 4 cats, but I feel that roughly 3 is the right #. What about you?");
    try testRender(template, .{.cats = 5}, "I have 5 cats, but I feel that roughly {perfect} is the right #. What about you?");
}

fn testRender(src: []const u8, args: anytype, expected: []const u8) !void {
    const Parser = @import("Parser.zig");

    var parser = try Parser.init(t.allocator, .{});
    defer parser.deinit();

    const msg = try parser.parseMessage(src);
    defer msg.deinit();

    var arr = std.ArrayList(u8).init(t.allocator);
    defer arr.deinit();

    try msg.render(arr.writer(), args);
    try t.expectString(expected, arr.items);
}
