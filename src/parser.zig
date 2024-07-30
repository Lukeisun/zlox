const std = @import("std");
const keywords = @import("main.zig").keywords;
const Expr = @import("expression.zig").Expr;
const TokenType = @import("token.zig").TokenType;
const Token = @import("token.zig").Token;
const Literal = @import("token.zig").Literal;
const Error = @import("error.zig").Error;
const Stmt = @import("statement.zig").Stmt;
const ParsingError = error{ UnexpectedToken, ExpectingExpr };
pub fn isParsingError(err: anyerror) bool {
    return switch (err) {
        ParsingError.ExpectingExpr, ParsingError.UnexpectedToken => true,
        else => false,
    };
}
pub const Parser = struct {
    current: u32 = 0,
    tokens: []Token,
    allocator: std.mem.Allocator,
    statements: *std.ArrayList(Stmt),
    pub fn create(allocator: std.mem.Allocator, tokens: []Token) Parser {
        const stmts_ptr = allocator.create(std.ArrayList(Stmt)) catch {
            std.debug.panic("OOM\n", .{});
        };
        const stmts = std.ArrayList(Stmt).init(allocator);
        stmts_ptr.* = stmts;
        return Parser{
            .allocator = allocator,
            .tokens = tokens,
            .statements = stmts_ptr,
        };
    }
    pub fn parse(allocator: std.mem.Allocator, tokens: []Token) *std.ArrayList(Stmt) {
        var parser = Parser.create(allocator, tokens);
        while (!parser.outOfBounds()) {
            const stmt = parser.declaration() catch |err| {
                if (isParsingError(err)) {
                    parser.statements.clearAndFree();
                    return parser.statements;
                } else {
                    std.debug.panic("{s}", .{@errorName(err)});
                }
            };
            parser.statements.append(stmt) catch |err| std.debug.panic("{s}", .{@errorName(err)});
        }
        return parser.statements;
    }
    fn expression(self: *Parser) anyerror!*Expr {
        return self.equality();
    }
    fn declaration(self: *Parser) !Stmt {
        errdefer {
            std.debug.print("in err defer\n", .{});
            self.synchronoize();
        }
        if (self.match(&[_]TokenType{TokenType.VAR})) {
            return self.varDeclaration();
        }
        return self.statement();
    }
    fn statement(self: *Parser) !Stmt {
        if (self.match(&[_]TokenType{TokenType.PRINT})) return self.printStatement();
        return self.expressionStatement();
    }
    fn printStatement(self: *Parser) !Stmt {
        const value = try self.expression();
        _ = self.consume(TokenType.SEMICOLON) catch |err| {
            return self.handleConsumeError(err, "Expecting ';' after expression");
        };
        return Stmt.Print.create(value);
    }
    fn expressionStatement(self: *Parser) !Stmt {
        const value = try self.expression();
        _ = self.consume(TokenType.SEMICOLON) catch |err| {
            return self.handleConsumeError(err, "Expecting ';' after expression");
        };
        return Stmt.Expression.create(value);
    }
    fn varDeclaration(self: *Parser) !Stmt {
        const name = self.consume(TokenType.IDENTIFIER) catch |err| {
            return self.handleConsumeError(err, "Expecting variable name");
        };
        var initializer: *Expr = undefined;
        if (self.match(&[_]TokenType{TokenType.EQUAL})) {
            initializer = try self.expression();
        } else {
            initializer = try Expr.Literal.create(self.allocator, .null);
        }
        _ = self.consume(TokenType.SEMICOLON) catch |err| {
            return self.handleConsumeError(err, "Expecting ';' after expression");
        };
        return Stmt.Var.create(name, initializer);
    }
    fn equality(self: *Parser) !*Expr {
        var expr = try self.comparison();
        while (self.match(&[_]TokenType{ TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL })) {
            const op = self.previous();
            const right = try self.comparison();
            expr = try Expr.Binary.create(self.allocator, expr, op, right);
        }
        return expr;
    }
    fn comparison(self: *Parser) !*Expr {
        var expr = try self.term();
        while (self.match(&[_]TokenType{ TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL })) {
            const op = self.previous();
            const right = try self.term();
            expr = try Expr.Binary.create(self.allocator, expr, op, right);
        }
        return expr;
    }
    fn term(self: *Parser) !*Expr {
        var expr = try self.factor();
        while (self.match(&[_]TokenType{ TokenType.MINUS, TokenType.PLUS })) {
            const op = self.previous();
            const right = try self.factor();
            expr = try Expr.Binary.create(self.allocator, expr, op, right);
        }
        return expr;
    }
    fn factor(self: *Parser) !*Expr {
        var expr = try self.unary();
        while (self.match(&[_]TokenType{ TokenType.SLASH, TokenType.STAR })) {
            const op = self.previous();
            const right = try self.unary();
            expr = try Expr.Binary.create(self.allocator, expr, op, right);
        }
        return expr;
    }
    fn unary(self: *Parser) !*Expr {
        if (self.match(&[_]TokenType{ TokenType.BANG, TokenType.MINUS })) {
            const op = self.previous();
            const right = try self.unary();
            return try Expr.Unary.create(self.allocator, op, right);
        }
        return self.primary();
    }
    fn primary(self: *Parser) !*Expr {
        if (self.match(&[_]TokenType{TokenType.FALSE})) return try Expr.Literal.create(self.allocator, Literal{ .boolean = false });
        if (self.match(&[_]TokenType{TokenType.TRUE})) return try Expr.Literal.create(self.allocator, Literal{ .boolean = true });
        if (self.match(&[_]TokenType{TokenType.NIL})) return try Expr.Literal.create(self.allocator, Literal{ .null = {} });
        if (self.match(&[_]TokenType{TokenType.IDENTIFIER})) {
            return try Expr.Variable.create(self.allocator, self.previous());
        }
        if (self.match(&[_]TokenType{ TokenType.NUMBER, TokenType.STRING })) {
            return try Expr.Literal.create(self.allocator, self.previous().literal);
        }
        if (self.match(&[_]TokenType{TokenType.LEFT_PAREN})) {
            const expr = try self.expression();
            _ = self.consume(TokenType.RIGHT_PAREN) catch |err| {
                return self.handleConsumeError(err, "Expecting ')' after expression");
            };
            return try Expr.Grouping.create(self.allocator, expr);
        }
        return ParsingError.ExpectingExpr;
    }

    fn match(self: *Parser, token_types: []const TokenType) bool {
        for (token_types) |token_type| {
            if (!self.check(token_type)) {
                continue;
            }
            _ = self.advance();
            return true;
        }
        return false;
    }
    fn check(self: *Parser, token_type: TokenType) bool {
        if (self.outOfBounds()) return false;
        return self.peek().type == token_type;
    }
    fn outOfBounds(self: *Parser) bool {
        return self.peek().type == TokenType.EOF;
    }
    fn advance(self: *Parser) Token {
        if (!self.outOfBounds()) {
            self.current += 1;
        }
        return self.previous();
    }
    fn previous(self: *Parser) Token {
        return self.tokens[self.current - 1];
    }
    fn peek(self: *Parser) Token {
        return self.tokens[self.current];
    }
    fn consume(self: *Parser, token_type: TokenType) !Token {
        if (self.check(token_type)) return self.advance();
        return ParsingError.UnexpectedToken;
    }
    fn synchronoize(self: *Parser) void {
        _ = self.advance();
        while (!self.outOfBounds()) {
            if (self.previous().type == TokenType.SEMICOLON) return;
            const needle = self.peek();
            for (keywords.values()) |keyword| {
                if (needle.type == keyword) return;
            }
            _ = self.advance();
        }
    }
    fn handleConsumeError(self: *Parser, err: anyerror, message: []const u8) anyerror {
        if (isParsingError(err)) {
            const t = self.peek();
            // var buf: [128]u8 = undefined;
            const where = "";
            const parse_error = Error{ .line = t.line, .where = where, .message = message };
            parse_error.report();
        } else {
            std.debug.print("{any}\n", .{@errorName(err)});
        }
        return err;
    }
};
