const std = @import("std");
const TokenType = enum(u8) {
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
const LexingError = error{
    UnknownCharacter,
};
const Error = struct {
    line: u32 = 0,
    where: []u8 = "",
    message: []u8 = "",
    pub fn report(self: Error) !void {
        const stdout = std.io.getStdOut().writer();
        stdout.print("[line: {d}] Error {s}: {s}", self.line, self.where, self.message);
    }
};
const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: u8,

    pub fn print(self: Token) !void {
        const stdout = std.io.getStdOut().writer();
        try stdout.print("{s}: {s} - Line: {d}\n", .{ @tagName(self.type), self.lexeme, self.line });
    }
};
const Lexer = struct {
    line: u8,
    start: u32,
    current: u32,
    tokens: *std.ArrayList(Token),
    source: []const u8,
    pub fn init(source: []const u8, tokens: *std.ArrayList(Token)) Lexer {
        return Lexer{ .line = 1, .start = 0, .current = 0, .tokens = tokens, .source = source };
    }
    pub fn outOfBounds(self: *Lexer) bool {
        return self.current >= self.source.len;
    }
    pub fn eatToken(self: *Lexer) !void {
        const c = self.source[self.current];
        self.current = self.current + 1;
        switch (c) {
            '{' => try self.addToken(TokenType.LEFT_BRACE),
            // else => return LexingError.UnknownCharacter,
            // TODO: Delete
            else => std.debug.print("NOP\n", .{}),
        }
    }
    pub fn addToken(self: *Lexer, token_type: TokenType) !void {
        const lexeme = if (self.outOfBounds()) "" else self.source[self.start..self.current];
        std.debug.print("{s}", .{lexeme});
        try self.tokens.append(.{ .type = token_type, .line = self.line, .lexeme = lexeme });
    }
};
pub fn lex(allocator: std.mem.Allocator, source: []const u8) !std.ArrayList(Token) {
    // const stdout = std.io.getStdOut().writer();
    var tokens = std.ArrayList(Token).init(allocator);
    var lexer = Lexer.init(source, &tokens);
    while (!lexer.outOfBounds()) {
        lexer.start = lexer.current;
        try lexer.eatToken();
    }
    std.debug.print("Ate all tokens 😋\n", .{});
    try lexer.addToken(TokenType.EOF);
    // try stdout.writeAll(source);
    return tokens;
}
pub fn runFile(allocator: std.mem.Allocator, filename: [:0]const u8) !void {
    // const stdout = std.io.getStdOut().writer();
    const file = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    const stat = try file.stat();
    const buff = try file.readToEndAlloc(allocator, stat.size);
    // for (buff) |c| {
    //     try stdout.print("{c}", .{c});
    // }
    // try stdout.writeAll(buff);
    const tokens = try lex(allocator, buff);
    defer tokens.deinit();
    for (tokens.items) |token| {
        try token.print();
    }
}
pub fn runPrompt(allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.writeAll("> ");
    while (try stdin.readUntilDelimiterOrEofAlloc(allocator, '\n', 128)) |s| {
        defer allocator.free(s);
        try stdout.print("{s}\n", .{s});
        try stdout.writeAll("> ");
    }
}
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    var collected_args = std.ArrayList([:0]const u8).init(allocator);
    _ = args.skip();
    while (args.next()) |arg| {
        try collected_args.append(arg);
    }
    switch (collected_args.items.len) {
        1 => {
            try runFile(allocator, collected_args.items[0]);
        },
        0 => {
            try runPrompt(allocator);
        },
        else => {
            const stderr = std.io.getStdErr().writer();
            try stderr.writeAll("Usage: zlox [script]\n");
        },
    }
}
