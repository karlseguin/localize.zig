const std = @import("std");
const Allocator = std.mem.Allocator;

const Data = @This();

_inner: std.StringHashMapUnmanaged(Value) = .{},

pub fn initFromStruct(allocator: Allocator, s: anytype) !Data {
    const T = @TypeOf(s);

    switch (@typeInfo(T)) {
        .null => return .{},
        .@"struct" => |str| {
            const fields = str.fields;

            var map: std.StringHashMapUnmanaged(Value) = .{};
            try map.ensureTotalCapacity(allocator, fields.len);

            inline for (fields) |f| {
                map.putAssumeCapacity(f.name, Value.new(@field(s, f.name)));
            }

            return .{ ._inner = map };
        },
        else => @compileError("Must be a struct, got: " ++ @typeName(T)),
    }
}

pub fn deinit(self: *Data, allocator: Allocator) void {
    self._inner.deinit(allocator);
}

pub fn get(self: *const Data, key: []const u8) ?Value {
    return self._inner.get(key);
}

pub fn jsonStringify(self: *const Data, jws: anytype) !void {
    try jws.beginObject();
    var it = self._inner.iterator();
    while (it.next()) |kv| {
        try jws.objectField(kv.key_ptr.*);
        switch (kv.value_ptr.*) {
            .null => try jws.write(null),
            inline else => |v| try jws.write(v),
        }
    }
    return jws.endObject();
}

pub const Value = union(enum) {
    null: void,
    u64: u64,
    i64: i64,
    f32: f32,
    f64: f64,
    bool: bool,
    string: []const u8,

    pub fn new(value: anytype) Value {
        const T = @TypeOf(value);
        switch (@typeInfo(T)) {
            .null => return .{ .null = {} },
            .comptime_int => return .{ .i64 = value },
            .int => |int| {
                if (int.signedness == .signed) {
                    switch (int.bits) {
                        1...64 => return .{ .i64 = @intCast(value) },
                        else => {},
                    }
                } else {
                    switch (int.bits) {
                        1...64 => return .{ .u64 = @intCast(value) },
                        else => {},
                    }
                }
            },
            .float => |float| {
                switch (float.bits) {
                    1...32 => return .{ .f32 = value },
                    33...64 => return .{ .f64 = value },
                    else => return error.UnsupportedValueType,
                }
            },
            .comptime_float => return .{ .f64 = @floatCast(value) },
            .bool => return .{ .bool = value },
            .array => @compileError("String values must be slices"),
            .pointer => if (comptime isString(T)) {
                return .{ .string = value };
            },
            .@"enum" => return .{ .string = @tagName(value) },
            .optional => |opt| {
                if (value) |v| {
                    return new(@as(opt.child, v));
                }
                return .{ .null = {} };
            },
            else => {},
        }
        @compileLog(@typeInfo(T).pointer.child);
        @compileError("Unsupported value type: " ++ @typeName(T));
    }

    pub fn write(self: Value, writer: anytype) !void {
        switch (self) {
            .null => return writer.writeAll("null"),
            .u64 => |v| return std.fmt.formatInt(v, 10, .lower, .{}, writer),
            .i64 => |v| return std.fmt.formatInt(v, 10, .lower, .{}, writer),
            .f64 => |v| return std.fmt.format(writer, "{d}", .{v}),
            .f32 => |v| return std.fmt.format(writer, "{d}", .{v}),
            .bool => |v| return writer.writeAll(if (v) "true" else "false"),
            .string => |v| return writer.writeAll(v),
        }
    }
};

fn isString(comptime T: type) bool {
    return comptime blk: {
        // Only pointer types can be strings, no optionals
        const info = @typeInfo(T);
        if (info != .pointer) break :blk false;

        const ptr = &info.pointer;
        // Check for CV qualifiers that would prevent coerction to []const u8
        if (ptr.is_volatile or ptr.is_allowzero) break :blk false;

        // If it's already a slice, simple check.
        if (ptr.size == .Slice) {
            break :blk ptr.child == u8;
        }

        // Otherwise check if it's an array type that coerces to slice.
        if (ptr.size == .One) {
            const child = @typeInfo(ptr.child);
            if (child == .array) {
                const arr = &child.array;
                break :blk arr.child == u8;
            }
        }

        break :blk false;
    };
}

const t = @import("t.zig");
test "Data: initFromStruct" {
    const str = "123";
    const str3 = try t.allocator.dupe(u8, str);

    var data = try Data.initFromStruct(t.allocator, .{
        .int_ct = 12345,
        .int_u8 = @as(u8, 123),
        .int_u16 = @as(u16, 4566),
        .int_u32 = @as(u32, 58293845),
        .int_u64 = @as(u64, 32838128384),
        .int_i8 = @as(i8, -123),
        .int_i16 = @as(i16, -4566),
        .int_i32 = @as(i32, -58293845),
        .int_i64 = @as(i64, -32838128384),
        .int_ft = 123.456889,
        .f_32 = @as(f32, 0.910),
        .f_64 = @as(f64, -59924592.334912),
        .str_2 = str[0..],
        .str_3 = str3,
        .bool_t = true,
        .bool_f = false,
        .null_1 = null,
        .null_2 = @as(?[]const u8, null),
        .null_3 = @as(?i33, 234),
    });
    defer t.allocator.free(str3);
    defer data.deinit(t.allocator);

    try t.expectEqual(12345, data.get("int_ct").?.i64);
    try t.expectEqual(123, data.get("int_u8").?.u64);
    try t.expectEqual(4566, data.get("int_u16").?.u64);
    try t.expectEqual(58293845, data.get("int_u32").?.u64);
    try t.expectEqual(32838128384, data.get("int_u64").?.u64);
    try t.expectEqual(-123, data.get("int_i8").?.i64);
    try t.expectEqual(-4566, data.get("int_i16").?.i64);
    try t.expectEqual(-58293845, data.get("int_i32").?.i64);
    try t.expectEqual(-32838128384, data.get("int_i64").?.i64);

    try t.expectEqual(9.100000262260437e-1, data.get("f_32").?.f32);
    try t.expectEqual(-59924592.334912, data.get("f_64").?.f64);

    try t.expectEqual(true, data.get("bool_t").?.bool);
    try t.expectEqual(false, data.get("bool_f").?.bool);

    try t.expectString("123", data.get("str_2").?.string);
    try t.expectString("123", data.get("str_3").?.string);

    try t.expectEqual({}, data.get("null_2").?.null);
    try t.expectEqual({}, data.get("null_2").?.null);
    try t.expectEqual(234, data.get("null_3").?.i64);
}

test "Data: jsonStringify" {
    var data = try Data.initFromStruct(t.allocator, .{
        .int_ct = 12345,
        .int_u8 = @as(u8, 123),
        .int_u16 = @as(u16, 4566),
        .int_u32 = @as(u32, 58293845),
        .int_u64 = @as(u64, 32838128384),
        .int_i8 = @as(i8, -123),
        .int_i16 = @as(i16, -4566),
        .int_i32 = @as(i32, -58293845),
        .int_i64 = @as(i64, -32838128384),
        .int_ft = 123.456889,
        .f_32 = @as(f32, 0.910),
        .f_64 = @as(f64, -59924592.334912),
        .str_1 = "123",
        .bool_t = true,
        .bool_f = false,
        .null_1 = null,
        .null_2 = @as(?[]const u8, null),
        .null_3 = @as(?i33, 234),
    });
    defer data.deinit(t.allocator);

    const json_string = try std.json.stringifyAlloc(t.allocator, data, .{});
    defer t.allocator.free(json_string);

    var parsed = try std.json.parseFromSlice(std.json.Value, t.allocator, json_string, .{});
    defer parsed.deinit();

    const value = parsed.value.object;
    try t.expectEqual(12345, value.get("int_ct").?.integer);
    try t.expectEqual(123, value.get("int_u8").?.integer);
    try t.expectEqual(4566, value.get("int_u16").?.integer);
    try t.expectEqual(58293845, value.get("int_u32").?.integer);
    try t.expectEqual(32838128384, value.get("int_u64").?.integer);
    try t.expectEqual(-123, value.get("int_i8").?.integer);
    try t.expectEqual(-4566, value.get("int_i16").?.integer);
    try t.expectEqual(-58293845, value.get("int_i32").?.integer);
    try t.expectEqual(-32838128384, value.get("int_i64").?.integer);
    try t.expectEqual(9.100000262260437e-1, value.get("f_32").?.float);
    try t.expectEqual(-59924592.334912, value.get("f_64").?.float);
    try t.expectString("123", value.get("str_1").?.string);
    try t.expectEqual(true, value.get("bool_t").?.bool);
    try t.expectEqual(false, value.get("bool_f").?.bool);
    try t.expectEqual({}, value.get("null_1").?.null);
    try t.expectEqual({}, value.get("null_2").?.null);
    try t.expectEqual(234, value.get("null_3").?.integer);
}
