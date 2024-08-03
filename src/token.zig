const std = @import("std");
const Callable = @import("callable.zig").Callable;
pub const TokenType = enum(u8) {
    // Single Character Tokens
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    // One/Two char tokens
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    // Literals
    IDENTIFIER,
    STRING,
    NUMBER,
    // Keywords
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    EOF,
};
pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: u16,
    literal: Literal,

    pub fn print(self: Token) !void {
        std.debug.print("{s} - Line {d}:\n\tLexeme: {s}\tLiteral: ", .{ @tagName(self.type), self.line, self.lexeme });
        var buf: [128]u8 = undefined;
        std.debug.print("{s}\n", .{try self.literal.toString(&buf)});
    }
};
pub fn debugTokens(tokens: []Token) !void {
    for (tokens) |t| {
        try t.print();
    }
}
pub const Literal = union(enum) {
    string: []const u8,
    number: f32,
    boolean: bool,
    callable: *Callable,
    null,
    pub fn toString(self: Literal, buf: []u8) ![]const u8 {
        switch (self) {
            .string => |s| return s,
            .number => |num| {
                return try std.fmt.bufPrint(buf, "{d}", .{num});
            },
            .boolean => |b| return if (b) "true" else "false",
            .null => return "null",
            .callable => return "callable",
        }
    }
    pub fn toStringAlloc(self: Literal, allocator: std.mem.Allocator) ![]const u8 {
        switch (self) {
            .string => |s| return s,
            .number => |num| {
                return try std.fmt.allocPrint(allocator, "{d}", .{num});
            },
            .boolean => |b| return if (b) "true" else "false",
            .null => return "null",
            .callable => return "callable",
        }
    }
    pub fn tagEquals(self: Literal, o: Literal) bool {
        return std.meta.activeTag(self) == std.meta.activeTag(o);
    }
};
