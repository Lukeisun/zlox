const std = @import("std");
const token = @import("token.zig");
const LexingError = error{
    UnknownCharacter,
    UnterminatedString,
};
// TODO: move
const Error = struct {
    line: u32 = 0,
    where: []u8 = "",
    message: [:0]const u8 = "",
    pub fn report(self: Error) void {
        std.debug.print("[line: {d}] Error {s}: {s}\n", .{ self.line, self.where, self.message });
    }
};
const Lexer = struct {
    line: u16,
    start: u32,
    current: u32,
    tokens: *std.ArrayList(token.Token),
    source: []const u8,
    keywords: *std.StringHashMap(token.TokenType),
    pub fn init(source: []const u8, tokens: *std.ArrayList(token.Token), keywords: *std.StringHashMap(token.TokenType)) Lexer {
        return Lexer{ .line = 1, .start = 0, .current = 0, .tokens = tokens, .source = source, .keywords = keywords };
    }
    pub fn outOfBounds(self: *Lexer) bool {
        return self.current >= self.source.len;
    }
    pub fn eatToken(self: *Lexer) !void {
        const c = self.source[self.current];
        self.current += 1;
        switch (c) {
            '{' => try self.addToken(token.TokenType.LEFT_BRACE),
            '}' => try self.addToken(token.TokenType.RIGHT_BRACE),
            '(' => try self.addToken(token.TokenType.LEFT_PAREN),
            ')' => try self.addToken(token.TokenType.RIGHT_PAREN),
            ',' => try self.addToken(token.TokenType.COMMA),
            '.' => try self.addToken(token.TokenType.DOT),
            '-' => try self.addToken(token.TokenType.MINUS),
            '+' => try self.addToken(token.TokenType.PLUS),
            ';' => try self.addToken(token.TokenType.SEMICOLON),
            '*' => try self.addToken(token.TokenType.STAR),
            '!' => {
                try if (self.match('=')) self.addToken(token.TokenType.BANG) else self.addToken(token.TokenType.BANG);
            },
            '=' => {
                try if (self.match('=')) self.addToken(token.TokenType.EQUAL_EQUAL) else self.addToken(token.TokenType.EQUAL);
            },
            '>' => {
                try if (self.match('=')) self.addToken(token.TokenType.GREATER_EQUAL) else self.addToken(token.TokenType.GREATER);
            },
            '<' => {
                try if (self.match('=')) self.addToken(token.TokenType.LESS_EQUAL) else self.addToken(token.TokenType.LESS);
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
                    }
                } else {
                    try self.addToken(token.TokenType.SLASH);
                }
            },
            '"' => try self.string(),
            ' ', '\r', '\t' => {},
            '\n' => self.line += 1,
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
    pub fn addTokenWithLiteral(self: *Lexer, token_type: token.TokenType, literal: token.Literal) !void {
        const lexeme = if (self.outOfBounds()) "" else self.source[self.start..self.current];
        try self.tokens.append(.{ .type = token_type, .line = self.line, .lexeme = lexeme, .literal = literal });
    }
    pub fn addToken(self: *Lexer, token_type: token.TokenType) !void {
        const lexeme = if (self.outOfBounds()) "" else self.source[self.start..self.current];
        try self.tokens.append(.{ .type = token_type, .line = self.line, .lexeme = lexeme, .literal = token.Literal.null });
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
            try self.addToken(token.TokenType.IDENTIFIER);
        }
    }
    pub fn number(self: *Lexer) !void {
        while (std.ascii.isDigit(self.peek())) self.current += 1;
        if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
            self.current += 1;
            while (std.ascii.isDigit(self.peek())) self.current += 1;
        }
        const float = try std.fmt.parseFloat(f32, self.source[self.start..self.current]);
        const literal = token.Literal{ .number = float };
        try self.addTokenWithLiteral(token.TokenType.NUMBER, literal);
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
        const literal = token.Literal{ .string = self.source[self.start + 1 .. self.current - 1] };
        try self.addTokenWithLiteral(token.TokenType.STRING, literal);
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
pub fn lex(allocator: std.mem.Allocator, source: []const u8) !std.ArrayList(token.Token) {
    var tokens = std.ArrayList(token.Token).init(allocator);
    var map = std.StringHashMap(token.TokenType).init(allocator);
    try map.put("and", token.TokenType.AND);
    try map.put("class", token.TokenType.CLASS);
    try map.put("else", token.TokenType.ELSE);
    try map.put("false", token.TokenType.FALSE);
    try map.put("for", token.TokenType.FOR);
    try map.put("fun", token.TokenType.FUN);
    try map.put("if", token.TokenType.IF);
    try map.put("nil", token.TokenType.NIL);
    try map.put("or", token.TokenType.OR);
    try map.put("print", token.TokenType.PRINT);
    try map.put("return", token.TokenType.RETURN);
    try map.put("super", token.TokenType.SUPER);
    try map.put("this", token.TokenType.THIS);
    try map.put("true", token.TokenType.TRUE);
    try map.put("var", token.TokenType.VAR);
    try map.put("while", token.TokenType.WHILE);
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
    try lexer.addToken(token.TokenType.EOF);
    return tokens;
}
