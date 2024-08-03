const std = @import("std");
const keywords = @import("main.zig").keywords;
const Expr = @import("expression.zig").Expr;
const TokenType = @import("token.zig").TokenType;
const Token = @import("token.zig").Token;
const Literal = @import("token.zig").Literal;
const Error = @import("error.zig").Error;
const Stmt = @import("statement.zig").Stmt;
const ParsingError = error{ UnexpectedToken, ExpectingExpr };
const ParserError = ParsingError || error{OutOfMemory};
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
    statements: std.ArrayList(*Stmt),
    pub fn create(allocator: std.mem.Allocator, tokens: []Token) Parser {
        const stmts = std.ArrayList(*Stmt).init(allocator);
        return Parser{
            .allocator = allocator,
            .tokens = tokens,
            .statements = stmts,
        };
    }
    pub fn parse(allocator: std.mem.Allocator, tokens: []Token) []*Stmt {
        var parser = Parser.create(allocator, tokens);
        while (!parser.outOfBounds()) {
            const stmt = parser.declaration() catch |err| {
                if (isParsingError(err)) {
                    parser.statements.clearAndFree();
                    return parser.statements.toOwnedSlice() catch {
                        std.debug.panic("OOM\n", .{});
                    };
                } else {
                    std.debug.panic("{s}", .{@errorName(err)});
                }
            };
            parser.statements.append(stmt) catch |err| std.debug.panic("{s}", .{@errorName(err)});
        }
        return parser.statements.toOwnedSlice() catch {
            std.debug.panic("OOM\n", .{});
        };
    }
    fn declaration(self: *Parser) ParserError!*Stmt {
        errdefer {
            std.debug.print("in err defer\n", .{});
            self.synchronoize();
        }
        if (self.match(&[_]TokenType{TokenType.VAR})) {
            return self.varDeclaration();
        }
        return self.statement();
    }
    fn statement(self: *Parser) ParserError!*Stmt {
        if (self.match(&[_]TokenType{TokenType.PRINT})) return self.printStatement();
        if (self.match(&[_]TokenType{TokenType.IF})) return self.ifStatement();
        if (self.match(&[_]TokenType{TokenType.WHILE})) return self.whileStatement();
        if (self.match(&[_]TokenType{TokenType.FOR})) return self.forStatement();
        if (self.match(&[_]TokenType{TokenType.LEFT_BRACE})) {
            const stmt_list = try self.block();
            return Stmt.Block.create(self.allocator, stmt_list);
        }
        return self.expressionStatement();
    }
    fn ifStatement(self: *Parser) !*Stmt {
        _ = try self.consume(TokenType.LEFT_PAREN, "Expecting '(' after 'if'");
        const condition = try self.expression();
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expecting ')' after if condition");
        const then_branch = try self.statement();
        var else_branch: ?*Stmt = null;
        if (self.match(&[_]TokenType{TokenType.ELSE})) {
            else_branch = try self.statement();
        }
        return try Stmt.If.create(self.allocator, condition, then_branch, else_branch);
    }

    fn printStatement(self: *Parser) !*Stmt {
        const value = try self.expression();
        _ = try self.consume(TokenType.SEMICOLON, "Expecting ';' after expression");
        return Stmt.Print.create(self.allocator, value);
    }
    fn block(self: *Parser) ![]*Stmt {
        var statements = std.ArrayList(*Stmt).init(self.allocator);
        while (!self.check(TokenType.RIGHT_BRACE) and !self.outOfBounds()) {
            const stmt = try self.declaration();
            try statements.append(stmt);
        }
        _ = try self.consume(TokenType.RIGHT_BRACE, "Expect '}' after block");
        return statements.toOwnedSlice();
    }
    fn varDeclaration(self: *Parser) ParserError!*Stmt {
        const name = try self.consume(TokenType.IDENTIFIER, "Expecting variable name");
        var initializer: *Expr = undefined;
        if (self.match(&[_]TokenType{TokenType.EQUAL})) {
            initializer = try self.expression();
        } else {
            initializer = try Expr.Literal.create(self.allocator, .null);
        }
        _ = try self.consume(TokenType.SEMICOLON, "Expecting ';' after expression");
        return Stmt.Var.create(self.allocator, name, initializer);
    }
    fn whileStatement(self: *Parser) !*Stmt {
        _ = try self.consume(TokenType.LEFT_PAREN, "Expecting '(' after 'while'");
        const condition = try self.expression();
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expecting ')' after while condition");
        const body = try self.statement();
        return Stmt.While.create(self.allocator, condition, body);
    }
    fn forStatement(self: *Parser) !*Stmt {
        _ = try self.consume(TokenType.LEFT_PAREN, "Expecting '(' after 'for'");
        var initializer: ?*Stmt = undefined;
        if (self.match(&[_]TokenType{TokenType.SEMICOLON})) {
            initializer = null;
        } else if (self.match(&[_]TokenType{TokenType.VAR})) {
            initializer = try self.varDeclaration();
        } else {
            initializer = try self.expressionStatement();
        }
        var condition: ?*Expr = null;
        if (self.check(TokenType.SEMICOLON)) {
            condition = try self.expression();
        }
        _ = try self.consume(TokenType.SEMICOLON, "Expecting ';' after for clauses");
        // const condition = try self.expression();
        _ = try self.consume(TokenType.RIGHT_PAREN, "Expecting ')' after for clauses");
        return initializer.?;
    }
    fn expressionStatement(self: *Parser) !*Stmt {
        const value = try self.expression();
        _ = try self.consume(TokenType.SEMICOLON, "Expecting ';' after expression");
        return Stmt.Expression.create(self.allocator, value);
    }
    fn expression(self: *Parser) ParserError!*Expr {
        return self.assignment();
    }
    fn assignment(self: *Parser) ParserError!*Expr {
        const expr = try self._or();
        if (self.match(&[_]TokenType{TokenType.EQUAL})) {
            const equals = self.previous();
            const value = try self.assignment();
            switch (expr.*) {
                .variable => |v| {
                    const name = v.name;
                    return try Expr.Assign.create(self.allocator, name, value);
                },
                else => {
                    const where = "";
                    const parse_error = Error{ .line = equals.line, .where = where, .message = "Invalid Assignment Target" };
                    parse_error.report();
                },
            }
        }
        return expr;
    }
    fn _or(self: *Parser) ParserError!*Expr {
        var expr = try self._and();
        const match_arr = [_]TokenType{TokenType.OR};
        while (self.match(&match_arr)) {
            const op = self.previous();
            const right = try self._and();
            expr = try Expr.Logical.create(self.allocator, expr, op, right);
        }
        return expr;
    }
    fn _and(self: *Parser) ParserError!*Expr {
        var expr = try self.equality();
        const match_arr = [_]TokenType{TokenType.AND};
        while (self.match(&match_arr)) {
            const op = self.previous();
            const right = try self.equality();
            expr = try Expr.Logical.create(self.allocator, expr, op, right);
        }
        return expr;
    }
    fn equality(self: *Parser) ParserError!*Expr {
        var expr = try self.comparison();
        const match_arr = [_]TokenType{ TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL };
        while (self.match(&match_arr)) {
            const op = self.previous();
            const right = try self.comparison();
            expr = try Expr.Binary.create(self.allocator, expr, op, right);
        }
        return expr;
    }
    fn comparison(self: *Parser) ParserError!*Expr {
        var expr = try self.term();
        const match_arr = [_]TokenType{ TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL };
        while (self.match(&match_arr)) {
            const op = self.previous();
            const right = try self.term();
            expr = try Expr.Binary.create(self.allocator, expr, op, right);
        }
        return expr;
    }
    fn term(self: *Parser) ParserError!*Expr {
        var expr = try self.factor();
        const match_arr = [_]TokenType{ TokenType.MINUS, TokenType.PLUS };
        while (self.match(&match_arr)) {
            const op = self.previous();
            const right = try self.factor();
            expr = try Expr.Binary.create(self.allocator, expr, op, right);
        }
        return expr;
    }
    fn factor(self: *Parser) ParserError!*Expr {
        var expr = try self.unary();
        const match_arr = [_]TokenType{ TokenType.SLASH, TokenType.STAR };
        while (self.match(&match_arr)) {
            const op = self.previous();
            const right = try self.unary();
            expr = try Expr.Binary.create(self.allocator, expr, op, right);
        }
        return expr;
    }
    fn unary(self: *Parser) ParserError!*Expr {
        const match_arr = [_]TokenType{ TokenType.BANG, TokenType.MINUS };
        if (self.match(&match_arr)) {
            const op = self.previous();
            const right = try self.unary();
            return try Expr.Unary.create(self.allocator, op, right);
        }
        return self.primary();
    }
    fn primary(self: *Parser) ParserError!*Expr {
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
            _ = try self.consume(TokenType.RIGHT_PAREN, "Expecting ')' after expression");
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
    fn consume(self: *Parser, token_type: TokenType, message: []const u8) !Token {
        if (self.check(token_type)) return self.advance();
        const t = self.peek();
        const where = "";
        const parse_error = Error{ .line = t.line, .where = where, .message = message };
        parse_error.report();
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
};
