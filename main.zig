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
    UnterminatedString,
};
const Error = struct {
    line: u32 = 0,
    where: []u8 = "",
    message: [:0]const u8 = "",
    pub fn report(self: Error) void {
        std.debug.print("[line: {d}] Error {s}: {s}\n", .{ self.line, self.where, self.message });
    }
};
pub const Literal = union(enum) { string: []const u8, number: f32, null };
const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: u16,
    literal: Literal,

    pub fn print(self: Token) !void {
        // const stdout = std.io.getStdOut().writer();
        std.debug.print("{s} - Line {d}:\n\tLexeme: {s}\n\tLiteral: ", .{ @tagName(self.type), self.line, self.lexeme });
        switch (self.literal) {
            .string => std.debug.print("{s}", .{self.literal.string}),
            .number => std.debug.print("{d}", .{self.literal.number}),
            .null => std.debug.print("{?}", .{self.literal.null}),
        }
        std.debug.print("\n", .{});
        // try stdout.print("{s}: Lexeme: {s} Literal: {?} - Line: {d}\n", .{ @tagName(self.type), self.lexeme, self.literal, self.line });
    }
};
const Lexer = struct {
    line: u16,
    start: u32,
    current: u32,
    tokens: *std.ArrayList(Token),
    source: []const u8,
    keywords: *std.StringHashMap(TokenType),
    pub fn init(source: []const u8, tokens: *std.ArrayList(Token), keywords: *std.StringHashMap(TokenType)) Lexer {
        return Lexer{ .line = 1, .start = 0, .current = 0, .tokens = tokens, .source = source, .keywords = keywords };
    }
    pub fn outOfBounds(self: *Lexer) bool {
        return self.current >= self.source.len;
    }
    pub fn eatToken(self: *Lexer) !void {
        const c = self.source[self.current];
        self.current += 1;
        switch (c) {
            '{' => try self.addToken(TokenType.LEFT_BRACE),
            '}' => try self.addToken(TokenType.RIGHT_BRACE),
            '(' => try self.addToken(TokenType.LEFT_PAREN),
            ')' => try self.addToken(TokenType.RIGHT_PAREN),
            ',' => try self.addToken(TokenType.COMMA),
            '.' => try self.addToken(TokenType.DOT),
            '-' => try self.addToken(TokenType.MINUS),
            '+' => try self.addToken(TokenType.PLUS),
            ';' => try self.addToken(TokenType.SEMICOLON),
            '*' => try self.addToken(TokenType.STAR),
            '!' => {
                try if (self.match('=')) self.addToken(TokenType.BANG) else self.addToken(TokenType.BANG);
            },
            '=' => {
                try if (self.match('=')) self.addToken(TokenType.EQUAL_EQUAL) else self.addToken(TokenType.EQUAL);
            },
            '>' => {
                try if (self.match('=')) self.addToken(TokenType.GREATER_EQUAL) else self.addToken(TokenType.GREATER);
            },
            '<' => {
                try if (self.match('=')) self.addToken(TokenType.LESS_EQUAL) else self.addToken(TokenType.LESS);
            },
            '/' => {
                if (self.match('/')) {
                    while (self.peek() != '\n' and !self.outOfBounds()) {
                        self.current += 1;
                    }
                    // challenge 4: c style strings
                } else if (self.match('*')) {
                    var num_closings: u8 = 1;
                    while (num_closings != 0 and !self.outOfBounds()) {
                        if (self.peek() == '/' and self.peekNext() == '*') {
                            self.current += 1;
                            num_closings += 1;
                        }
                        if (self.peek() == '*' and self.peekNext() == '/') {
                            self.current += 1;
                            num_closings -= 1;
                        }
                        if (self.peek() == '\n') self.line += 1;
                        self.current += 1;
                        std.debug.print("{d}\n", .{num_closings});
                    }
                } else {
                    try self.addToken(TokenType.SLASH);
                }
            },
            '"' => try self.string(),
            ' ', '\r', '\t' => {},
            '\n' => self.line += 1,
            // '' => {IDENTIFIER},
            // '' => {STRING},
            // '' => {NUMBER},
            // '' => {AND},
            // '' => {CLASS},
            // '' => {ELSE},
            // '' => {FALSE},
            // '' => {FUN},
            // '' => {FOR},
            // '' => {IF},
            // '' => {NIL},
            // '' => {OR},
            // '' => {PRINT},
            // '' => {RETURN},
            // '' => {SUPER},
            // '' => {THIS},
            // '' => {TRUE},
            // '' => {VAR},
            // '' => {WHILE},
            // '' => {EOF},
            else => {
                if (std.ascii.isDigit(c)) {
                    try self.number();
                } else if (std.ascii.isAlphabetic(c) or c == '_') {
                    try self.identifier();
                } else {
                    return LexingError.UnknownCharacter;
                }
            },
        }
    }
    pub fn addTokenWithLiteral(self: *Lexer, token_type: TokenType, literal: Literal) !void {
        const lexeme = if (self.outOfBounds()) "" else self.source[self.start..self.current];
        // std.debug.print("{s}", .{lexeme});
        try self.tokens.append(.{ .type = token_type, .line = self.line, .lexeme = lexeme, .literal = literal });
    }
    pub fn addToken(self: *Lexer, token_type: TokenType) !void {
        const lexeme = if (self.outOfBounds()) "" else self.source[self.start..self.current];
        // std.debug.print("{s}", .{lexeme});
        try self.tokens.append(.{ .type = token_type, .line = self.line, .lexeme = lexeme, .literal = Literal.null });
    }
    pub fn peek(self: *Lexer) u8 {
        // I'm not quite sure if this is right
        // it seems to be, in the acii table 0 is \0
        if (self.outOfBounds()) return 0;
        return self.source[self.current];
    }
    pub fn peekNext(self: *Lexer) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }
    pub fn identifier(self: *Lexer) !void {
        while (std.ascii.isAlphanumeric(self.peek())) self.current += 1;
        const slice = self.source[self.start..self.current];
        const token_type = self.keywords.get(slice);
        if (token_type) |t| {
            try self.addToken(t);
        } else {
            try self.addToken(TokenType.IDENTIFIER);
        }
    }
    pub fn number(self: *Lexer) !void {
        while (std.ascii.isDigit(self.peek())) self.current += 1;
        if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
            self.current += 1;
            while (std.ascii.isDigit(self.peek())) self.current += 1;
        }
        const float = try std.fmt.parseFloat(f32, self.source[self.start..self.current]);
        const literal = Literal{ .number = float };
        try self.addTokenWithLiteral(TokenType.NUMBER, literal);
    }
    pub fn string(self: *Lexer) !void {
        while (self.peek() != '"' and !self.outOfBounds()) {
            if (self.peek() == '\n') self.line += 1;
            self.current += 1;
        }
        if (self.outOfBounds()) {
            return LexingError.UnterminatedString;
        }
        self.current += 1;
        const literal = Literal{ .string = self.source[self.start + 1 .. self.current - 1] };
        try self.addTokenWithLiteral(TokenType.STRING, literal);
    }
    pub fn match(self: *Lexer, expected: u8) bool {
        if (self.outOfBounds() or
            self.source[self.current] != expected)
        {
            return false;
        }
        self.current += 1;
        return true;
    }
};
pub fn lex(allocator: std.mem.Allocator, source: []const u8) !std.ArrayList(Token) {
    var tokens = std.ArrayList(Token).init(allocator);
    var map = std.StringHashMap(TokenType).init(allocator);
    try map.put("and", TokenType.AND);
    try map.put("class", TokenType.CLASS);
    try map.put("else", TokenType.ELSE);
    try map.put("false", TokenType.FALSE);
    try map.put("for", TokenType.FOR);
    try map.put("fun", TokenType.FUN);
    try map.put("if", TokenType.IF);
    try map.put("nil", TokenType.NIL);
    try map.put("or", TokenType.OR);
    try map.put("print", TokenType.PRINT);
    try map.put("return", TokenType.RETURN);
    try map.put("super", TokenType.SUPER);
    try map.put("this", TokenType.THIS);
    try map.put("true", TokenType.TRUE);
    try map.put("var", TokenType.VAR);
    try map.put("while", TokenType.WHILE);
    var lexer = Lexer.init(source, &tokens, &map);
    // var has_error = false;
    while (!lexer.outOfBounds()) {
        lexer.start = lexer.current;
        lexer.eatToken() catch |err| {
            const lex_error = Error{ .line = lexer.line, .where = "", .message = @errorName(err) };
            lex_error.report();
        };
    }
    std.debug.print("Ate all tokens 😋\n", .{});
    try lexer.addToken(TokenType.EOF);
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
