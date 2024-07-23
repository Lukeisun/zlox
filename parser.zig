const std = @import("std");
const Expr = @import("expression.zig").Expr;
const TokenType = @import("token.zig").TokenType;
const Token = @import("token.zig").Token;
const Literal = @import("token.zig").Literal;
const Parser = struct {
    var current = 0;
    tokens: []Token,
    allocator: std.mem.Allocator,
    pub fn create(allocator: std.mem.Allocator, tokens: []Token) Parser {
        return Parser{ .allocator = allocator, .tokens = tokens };
    }
    pub fn expression(self: Parser) Expr {
        return self.equality();
    }
    fn equality(self: Parser) Expr {
        var expr = self.comparision();
        while (self.match([_]TokenType{ TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL })) {
            const op = self.previous();
            const right = self.comparsion();
            expr = Expr.Binary.create(self.allocator, expr, op, right);
        }
        return expr;
    }
    fn comparison(self: Parser) Expr {
        var expr = self.term();
        while (self.match([_]TokenType{ TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL })) {
            const op = self.previous();
            const right = self.term();
            expr = Expr.Binary.create(self.allocator, expr, op, right);
        }
        return expr;
    }
    fn term(self: Parser) Expr {
        var expr = self.factor();
        while (self.match([_]TokenType{ TokenType.MINUS, TokenType.PLUS })) {
            const op = self.previous();
            const right = self.factor();
            expr = Expr.Binary.create(self.allocator, expr, op, right);
        }
        return expr;
    }

    fn factor(self: Parser) Expr {
        var expr = self.unary();
        while (self.match([_]TokenType{ TokenType.SLASH, TokenType.STAR })) {
            const op = self.previous();
            const right = self.unary();
            expr = Expr.Binary.create(self.allocator, expr, op, right);
        }
        return expr;
    }
    fn unary(self: Parser) Expr {
        if (self.match([_]TokenType{ TokenType.BANG, TokenType.MINUS })) {
            const op = self.previous();
            const right = self.unary();
            return Expr.Unary.create(self.allocator, op, right);
        }
        return self.primary();
    }
    fn primary(self: Parser) Expr {
        if (self.match([_]TokenType{TokenType.FALSE})) return Expr.Literal.create(self.allocator, Literal{ .boolean = false });
        if (self.match([_]TokenType{TokenType.TRUE})) return Expr.Literal.create(self.allocator, Literal{ .boolean = true });
        if (self.match([_]TokenType{TokenType.NIL})) return Expr.Literal.create(self.allocator, Literal{ .null = null });
        if (self.match([_]TokenType{ TokenType.NUMBER, TokenType.STRING })) {
            return Expr.Literal.create(self.allocator, self.previous().literal);
        }
        if (self.match([_]TokenType{TokenType.LEFT_PAREN})) {
            const expr = self.expression();
            self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression");
            return Expr.Grouping.create(self.allocator, expr);
        }
    }

    fn match(self: Parser, token_types: []TokenType) bool {
        for (token_types) |token_type| {
            if (!self.check(token_type)) {
                continue;
            }
            self.advance();
            return true;
        }
        return false;
    }
    fn check(self: Parser, token_type: TokenType) bool {
        if (self.outOfBounds()) return false;
        return self.peek() == token_type;
    }
    fn outOfBounds(self: Parser) bool {
        return self.peek() == TokenType.EOF;
    }
    fn advance(self: Parser) Token {
        if (!self.outOfBounds()) self.current += 1;
        return self.previous();
    }
    fn previous(self: Parser) Token {
        return self.tokens[self.current - 1];
    }
    fn peek(self: Parser) Token {
        return self.tokens[self.current];
    }
};
