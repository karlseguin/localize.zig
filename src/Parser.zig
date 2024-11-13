const std = @import("std");
const Message = @import("Message.zig");

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const ParseMode = union(enum) {
    normal,
    plural: []const u8, // the variable name
};

const ParsePartResult = union(enum) {
    part: Message.Part,
    done: ?Message.Part,
};

pos: usize,
src: []const u8,
options: Options,

// Each message will have its own arena. The message (and its arena) may outlive
// the parser. This is the allocator which will be used for that arena.
allocator: Allocator,

// Used for any parser-specific allocations. I.e. stack & scratch;
arena: Allocator,

mode: ParseMode,

// Depth of nesting, this is an index into stack.items[depth]
depth: usize,

// When we create a message, we'll give it a []Message.Part. But when parsing
// we don't know the count, so we use an ArrayList (and then clone the .items
// using the Message's arena). We need a stack of these for nesting.
stack: std.ArrayListUnmanaged(std.ArrayListUnmanaged(Message.Part)),

// In order to unescape values, we'll need some temp space. Could be more efficient
// than this on a per-message basis, but when re-used across multiple messages
// (or even multiple languages), an ArrayList is pretty efficient.
scratch: std.ArrayList(u8),

const Parser = @This();

pub fn init(allocator: Allocator, options: Options) !Parser {
    const arena = try allocator.create(ArenaAllocator);
    arena.* = ArenaAllocator.init(allocator);

    const aa = arena.allocator();

    return .{
        .pos = 0,
        .src = "",
        .depth = 0,
        .stack = .{},
        .mode = .normal,
        .options = options,
        .arena = aa,
        .allocator = allocator,
        .scratch = std.ArrayList(u8).init(aa),
    };
}

pub fn deinit(self: *Parser) void {
    const arena: *std.heap.ArenaAllocator = @ptrCast(@alignCast(self.arena.ptr));
    arena.deinit();
    arena.child_allocator.destroy(arena);
}

pub fn parseMessage(self: *Parser, src: []const u8) !Message {
    self.pos = 0;
    self.depth = 0;
    self.src = src;
    self.mode = .normal;
    self.stack.clearRetainingCapacity();

    // This is the arena for the message. Allocations here will be long-lived
    // so we want to be efficient with this
    var message_arena = std.heap.ArenaAllocator.init(self.allocator);
    errdefer message_arena.deinit();

    const parts = try self.parseParts(message_arena.allocator());

    return .{
        .parts = parts,
        .arena = message_arena,
    };
}

fn parseParts(self: *Parser, message_allocator: Allocator) ParseError![]Message.Part {
    const allocator = self.arena;
    const depth = self.depth;
    if (depth == self.stack.items.len) {
        try self.stack.append(allocator, .{});
    }

    var parts = self.stack.items[depth];
    defer parts.clearRetainingCapacity();

    blk: while (true) {
        switch (try self.nextPart(message_allocator)) {
            .part => |part| try parts.append(allocator, part),
            .done => |part| {
                if (part) |p| {
                    try parts.append(allocator, p);
                }
                break :blk;
            }
        }

    }

    // shrink this to size
    const owned_parts = try message_allocator.alloc(Message.Part, parts.items.len);
    @memcpy(owned_parts, parts.items);
    return owned_parts;
}

fn nextPart(self: *Parser, message_allocator: Allocator) !ParsePartResult{
    const mode = self.mode;

    var src = self.src;
    var scratch = &self.scratch;
    scratch.clearRetainingCapacity();

    var i: usize = 0;
    while (i < src.len) {
        switch (src[i]) {
            '{' => {
                if (scratch.items.len > 0) {
                    self.src = src[i..];
                    return .{.part = try generateLiteral(message_allocator, scratch.items, "")};
                }
                self.src = src[i+1..]; // skip the opening {
                return .{.part = try self.parseVariable(message_allocator, .none)};
            },
            '\'' => {
                // this includes the opening quote
                const remaining = src.len - i;
                if (remaining == 1) {
                    self.src = "";
                    try scratch.append('\'');
                    return .{.done = try generateLiteral(message_allocator, scratch.items, src)};
                }

                const next_index = i + 1;
                const next = src[next_index];
                if (next == '\'' or next == '{') {
                    const end = std.mem.indexOfScalarPos(u8, src, next_index, '\'') orelse {
                        self.src = "";
                        return .{.done = try generateLiteral(message_allocator, scratch.items, src[next_index..])};
                    };
                    try scratch.appendSlice(src[next_index..end]);
                    i = end + 1;
                } else if (mode == .plural and (next == '}' or next == '#')) {
                    const end = std.mem.indexOfScalarPos(u8, src, next_index, '\'') orelse return error.InvalidPluralCondition;
                    try scratch.appendSlice(src[next_index..end]);
                    i = end + 1;
                } else {
                    // not a real escape
                    i = next_index;
                    try scratch.append('\'');
                }
            },
            else => |c| {
                switch (mode) {
                    .plural => |plural_variable| switch (c) {
                        '}' => {
                            self.src = src[i+1..];
                            return .{.done = try generateMaybeLiteral(message_allocator, scratch.items)};
                        },
                        '#' => {
                            if (try generateMaybeLiteral(message_allocator, scratch.items)) |lit| {
                                self.src = src[i..];
                                return .{.part = lit};
                            }
                            self.src = src[i+1..];
                            return .{.part = .{.variable = .{.name = plural_variable, .constraint = .numeric}}};
                        },
                        else => {},
                    },
                    .normal => {},
                }

                i += 1;
                try scratch.append(c);
            },
        }
    }

    self.src = "";
    return .{.done = try generateMaybeLiteral(message_allocator, scratch.items)};
}

fn generateLiteral(message_allocator: Allocator, scratch: []u8, rest: []const u8) !Message.Part {
    var lit = try message_allocator.alloc(u8, scratch.len + rest.len);
    @memcpy(lit[0..scratch.len], scratch);
    @memcpy(lit[scratch.len..], rest);
    return .{.literal = lit};
}

fn generateMaybeLiteral(message_allocator: Allocator, scratch: []u8) !?Message.Part{
    if (scratch.len == 0) {
        return null;
    }
    return try generateLiteral(message_allocator, scratch, "");
}

fn parseVariable(self: *Parser, message_allocator: Allocator, constraint: Message.Part.Variable.Constraint) !Message.Part {
    self.skipSpaces();
    const variable_name = self.nextToken() orelse return error.InvalidVariableName;
    if (variable_name.len == 0) {
        return error.InvalidVariableName;
    }

    self.skipSpaces();
    const owned_variable_name = try message_allocator.dupe(u8, variable_name);
    switch (self.consumeByte()) {
        '}' => return .{.variable = .{.name = owned_variable_name, .constraint = constraint}},
        ',' => return self.parseCondition(message_allocator, owned_variable_name),
        else => return error.InvalidVariableName,
    }
}

fn parseCondition(self: *Parser, message_allocator: Allocator, variable_name: []const u8) !Message.Part {
    self.skipSpaces();
    const condition = self.nextToken() orelse return error.InvalidConditionType;
    if (std.mem.eql(u8, condition, "plural")) {
        return self.parsePlural(message_allocator, variable_name);
    }
    return error.UnknownConditionType;
}

fn parsePlural(self: *Parser, message_allocator: Allocator, variable_name: []const u8) !Message.Part {
    self.skipSpaces();
    if (self.consumeByte() != ',') {
        return error.InvalidConditionSyntax;
    }

    var zero: ?[]Message.Part = null;
    var one: ?[]Message.Part = null;
    var other: []Message.Part = &.{};

    while (true) {
        self.skipSpaces();
        const equality = self.consumeIf('=');
        const token = self.nextToken() orelse {
            if (self.consumeIf('}')) {
                break;
            }
            return error.InvalidPluralCondition;
        };

        if (equality) {
            if (token.len != 1) {
                return error.InvalidPluralCondition;
            }
            switch (token[0]) {
                '0' => zero = try self.parsePluralBranch(message_allocator, variable_name),
                '1' => one = try self.parsePluralBranch(message_allocator, variable_name),
                else => return error.InvalidPluralCondition,
            }
        } else {
            if (std.mem.eql(u8, token, "zero")) {
                zero = try self.parsePluralBranch(message_allocator, variable_name);
            } else if (std.mem.eql(u8, token, "one")) {
                one = try self.parsePluralBranch(message_allocator, variable_name);
            } else if (std.mem.eql(u8, token, "other")) {
                other = try self.parsePluralBranch(message_allocator, variable_name);
            } else {
                return error.InvalidPluralCondition;
            }
        }
    }

    if (other.len == 0) {
        return error.PluralRequiresOther;
    }

    return .{.plural = .{
        .zero = zero,
        .one = one,
        .other = other,
        .variable = .{.name = variable_name, .constraint = .numeric}, // already owned by the message arena
    }};
}

fn parsePluralBranch(self: *Parser, message_allocator: Allocator, variable_name: []const u8) ParseError![]Message.Part {
    self.skipSpaces();
    if (self.consumeIf('{') == false) {
        return error.InvalidPluralBranch;
    }
    if (self.src.len == 0) {
        return error.InvalidPluralBranch;
    }

    const state = try self.nest(.{.plural = variable_name});
    defer self.unnest(state);
    return try self.parseParts(message_allocator);
}

fn consumeIf(self: *Parser, b: u8) bool {
    const src = self.src;
    if (src.len == 0 or src[0] != b) {
        return false;
    }
    self.src = src[1..];
    return true;
}

fn consumeByte(self: *Parser) u8 {
    const src = self.src;
    if (src.len == 0) {
        return 0;
    }
    self.src = src[1..];
    return src[0];
}

fn nextToken(self: *Parser) ?[]const u8 {
    const src = self.src;
    if (src.len == 0) {
        return null;
    }

    for (src, 0..) |c, i| {
        switch (c) {
            'a'...'z', 'A'...'Z', '0'...'9', '_', '-' => {},
            else => {
                if (i == 0) {
                    return null;
                }
                self.src = src[i..];
                return src[0..i];
            }
        }
    }
    return src;
}

fn skipSpaces(self: *Parser) void {
    self.src = std.mem.trimLeft(u8, self.src, &std.ascii.whitespace);
}

fn nest(self: *Parser, new_mode: ParseMode) error{NestingTooDeep}!NestState {
    const old_mode = self.mode;

    if (self.depth == self.options.max_depth) {
        return error.NestingTooDeep;
    }

    self.depth += 1;
    self.mode = new_mode;

    return .{.mode = old_mode};
}

const ParseError = error {
    OutOfMemory,
    InvalidVariableName,
    InvalidConditionType,
    UnknownConditionType,
    InvalidConditionSyntax,
    InvalidPluralCondition,
    UnterminatedPluralCondition,
    PluralRequiresOther,
    NestingTooDeep,
    InvalidPluralBranch,
};

fn unnest(self: *Parser, state: NestState) void {
    self.depth -= 1;
    self.mode = state.mode;
}

const NestState = struct {
    mode: ParseMode,
};

pub const Options = struct {
    max_depth: u8 = 10,
};

const t = @import("t.zig");
test "Parser: literal only" {
    try testParseMessage("", .{}, &.{});
    try testParseMessage("hello", .{}, &.{.{.literal = "hello"}});
    try testParseMessage("  hello  ", .{}, &.{.{.literal = "  hello  "}});

    try testParseMessage("hello '{' world", .{}, &.{.{.literal = "hello { world"}});
    try testParseMessage("hello '{ world", .{}, &.{.{.literal = "hello { world"}});

    try testParseMessage("'{'", .{}, &.{.{.literal = "{"}});
    try testParseMessage("'{", .{}, &.{.{.literal = "{"}});
}

test "Parser: variable" {
    try testParseMessageError("{", .{}, error.InvalidVariableName);
    try testParseMessageError("{name", .{}, error.InvalidVariableName);
    try testParseMessageError("{}", .{}, error.InvalidVariableName);
    try testParseMessageError("{na me}", .{}, error.InvalidVariableName);
    try testParseMessage("{a}", .{}, &.{.{.variable = .{.name = "a"}}});
    try testParseMessage("{1}", .{}, &.{.{.variable = .{.name = "1"}}});
    try testParseMessage("{name}", .{}, &.{.{.variable = .{.name = "name"}}});
    try testParseMessage("{_name_}", .{}, &.{.{.variable = .{.name = "_name_"}}});
    try testParseMessage("{ a }", .{}, &.{.{.variable = .{.name = "a"}}});
    try testParseMessage("{ 1 }", .{}, &.{.{.variable = .{.name = "1"}}});
    try testParseMessage("{ name }", .{}, &.{.{.variable = .{.name = "name"}}});
    try testParseMessage("{\ta\t}", .{}, &.{.{.variable = .{.name = "a"}}});
    try testParseMessage("{\t1\t}", .{}, &.{.{.variable = .{.name = "1"}}});
    try testParseMessage("{\tname\t}", .{}, &.{.{.variable = .{.name = "name"}}});
}

test "Parser: literal + variable" {
    try testParseMessageError("hello {", .{}, error.InvalidVariableName);
    try testParseMessageError("hello {name{", .{}, error.InvalidVariableName);
    try testParseMessage("hello {name}", .{}, &.{.{.literal = "hello "}, .{.variable = .{.name = "name"}}});
    try testParseMessage("hello {name}!", .{}, &.{.{.literal = "hello "}, .{.variable = .{.name = "name"}}, .{.literal = "!"}});
    try testParseMessage("{name} hello", .{}, &.{.{.variable = .{.name = "name"}}, .{.literal = " hello"}});
}

test "Parser: invalid conditions" {
    try testParseMessageError("{val,}", .{}, error.InvalidConditionType);
    try testParseMessageError("{val,unknown}", .{}, error.UnknownConditionType);
    try testParseMessageError("{val ,  \tunknown}", .{}, error.UnknownConditionType);
    try testParseMessageError("{val,2}", .{}, error.UnknownConditionType);
}

test "Parser: plural invalid" {
    try testParseMessageError("{val,plural =0 {zero}}", .{}, error.InvalidConditionSyntax);
    try testParseMessageError("{val,plural} ", .{}, error.InvalidConditionSyntax);
    try testParseMessageError("{val,plural,} ", .{}, error.PluralRequiresOther);
    try testParseMessageError("{val,plural, =1 {one}} ", .{}, error.PluralRequiresOther);
    try testParseMessageError("{val,plural,=a {what}} ", .{}, error.InvalidPluralCondition);
    try testParseMessageError("{val,plural,=123a {what}} ", .{}, error.InvalidPluralCondition);
    try testParseMessageError("{val,plural, unknown {what}} ", .{}, error.InvalidPluralCondition);
    try testParseMessageError("{val,plural, =0 ", .{}, error.InvalidPluralBranch);
    try testParseMessageError("{val,plural, =0 {", .{}, error.InvalidPluralBranch);
    try testParseMessageError("{val,plural, =0 {}", .{}, error.InvalidPluralCondition);
}

test "Parser: plural single branch" {
    try testParseMessage(
        \\{val, plural,
        \\  other {value other}
        \\}
    , .{}, &.{.{.plural = .{
        .variable = .{.name = "val"},
        .other = &.{.{.literal = "value other"}},
    }}});
}

test "Parser: plural multiple branches" {
    try testParseMessage(
        \\{val, plural,
        \\  =0 {value zero}
        \\  =1 {value one}
        \\  other {value other}
        \\}
    , .{}, &.{.{.plural = .{
        .variable = .{.name = "val"},
        .zero = &.{.{.literal = "value zero"}},
        .one = &.{.{.literal = "value one"}},
        .other = &.{.{.literal = "value other"}},
    }}});
}

test "Parser: plural named branches" {
    try testParseMessage(
        \\{val, plural,
        \\  zero {value zero}
        \\  one{value one}
        \\  other {value other}
        \\}
    , .{}, &.{.{.plural = .{
        .variable = .{.name = "val"},
        .zero = &.{.{.literal = "value zero"}},
        .one = &.{.{.literal = "value one"}},
        .other = &.{.{.literal = "value other"}},
    }}});
}

test "Parser: plural with variable" {
    try testParseMessage(
        \\{val, plural,
        \\  zero {value zero {cat}}
        \\  one{value {dog} one}
        \\  other { {no}value other}
        \\}
    , .{}, &.{.{.plural = .{
        .variable = .{.name = "val"},
        .zero = &.{.{.literal = "value zero "}, .{.variable = .{.name = "cat"}} },
        .one = &.{.{.literal = "value "}, .{.variable = .{.name = "dog"}}, .{.literal = " one"}, },
        .other = &.{.{.literal = " "}, .{.variable = .{.name = "no"}}, .{.literal = "value other"}, },
    }}});
}

test "Parser: plural with variables" {
    try testParseMessage(
        \\{val, plural,
        \\  zero {{cat}{dog}}
        \\  one{{a}value {b} one { c}}
        \\  other {a{b}{b}b}
        \\}
    , .{}, &.{.{.plural = .{
        .variable = .{.name = "val"},
        .zero = &.{.{.variable = .{.name = "cat"}}, .{.variable = .{.name = "dog"}} },
        .one = &.{.{.variable = .{.name = "a"}}, .{.literal = "value "}, .{.variable = .{.name = "b"}}, .{.literal = " one "}, .{.variable = .{.name = "c"}}},
        .other = &.{.{.literal = "a"}, .{.variable = .{.name = "b"}}, .{.variable = .{.name = "b"}}, .{.literal = "b"}},
    }}});
}

test "Parser: plural special variable" {
    try testParseMessage(
        \\{cats, plural,
        \\  zero {0 cats}
        \\  one{1 cat}
        \\  other {'#' of cats: #}
        \\}
    , .{}, &.{.{.plural = .{
        .variable = .{.name = "cats", .constraint = .numeric},
        .zero = &.{.{.literal = "0 cats"} },
        .one = &.{.{.literal = "1 cat"}},
        .other = &.{.{.literal = "# of cats: "}, .{.variable = .{.name = "cats", .constraint = .numeric}}},
    }}});
}

fn testParseMessageError(src: []const u8, options: Options, expected: anyerror) !void {
    var parser = try Parser.init(t.allocator, options);
    defer parser.deinit();

    const msg = parser.parseMessage(src) catch |err| {
        return t.expectEqual(expected, err);
    };
    msg.deinit();
    return error.NoError;
}

fn testParseMessage(src: []const u8, options: Options, expected: ?[]const Message.Part) !void {
    var parser = try Parser.init(t.allocator, options);
    defer parser.deinit();

    const message = try parser.parseMessage(src);
    defer message.deinit();

    return expectParts(expected, message.parts);
}

fn  expectParts(expected: ?[]const Message.Part, actual: ?[]const Message.Part,) anyerror!void {
    if (expected == null or actual == null) {
        return t.expectEqual(expected, actual);
    }
    try t.expectEqual(expected.?.len, actual.?.len);
    for (expected.?, actual.?) |e, a| {
        try expectPart(e, a);
    }
}

fn  expectPart(expected: Message.Part, actual: Message.Part) anyerror!void {
    try t.expectString(@tagName(expected), @tagName(actual));
    switch (actual) {
        .literal => |lit| try t.expectString(expected.literal, lit),
        .variable => |variable| {
            try t.expectString(expected.variable.name, variable.name);
            try t.expectEqual(expected.variable.constraint, variable.constraint);
        },
        .plural => |plural| {
            try t.expectString(expected.plural.variable.name, plural.variable.name);
            try t.expectEqual(.numeric, plural.variable.constraint);
            try expectParts(expected.plural.zero, plural.zero);
            try expectParts(expected.plural.one, plural.one);
            try expectParts(expected.plural.other, plural.other);
        }
    }
}
