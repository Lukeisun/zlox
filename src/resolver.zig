const std = @import("std");
const Expr = @import("expression.zig").Expr;
const _token = @import("token.zig");
const Literal = _token.Literal;
const TokenType = _token.TokenType;
const Error = @import("error.zig").Error;
const Token = _token.Token;
const Stmt = @import("statement.zig").Stmt;
const Environment = @import("environment.zig").Environment;
const RuntimeError = @import("error.zig").RuntimeError;
const Interpreter = @import("interpreter.zig").Interpreter;
const _callable = @import("callable.zig");
const LoxFunction = _callable.LoxFunction;
const Callable = _callable.Callable;

pub const Resolver = struct {
    pub const ExprReturnType = RuntimeError!Literal;
    interpreter: *Interpreter,
    scopes: std.ArrayList(std.StringHashMap(bool)),
    allocator: std.mem.Allocator,
    pub fn create(allocator: std.mem.Allocator, interpreter: *Interpreter) Resolver {
        const scopes = std.ArrayList(std.StringHashMap(bool)).init(allocator);
        return .{
            .allocator = allocator,
            .interpreter = interpreter,
            .scopes = scopes,
        };
    }
    pub fn visitBlockStmt(self: Resolver, stmt: *Stmt.Block) void {
        self.beginScope();
        self.reslove(stmt.statements);
        self.endScope();
    }
    pub fn visitExpressionStmt(self: Resolver, stmt: *Stmt.Expression) void {
        self.resolve(stmt.expression);
    }
    pub fn visitFunctionStmt(self: Resolver, stmt: *Stmt.Function) void {
        self.declare(stmt.name);
        self.define(stmt.name);
        self.resolveFunction(stmt);
    }
    pub fn visitIfStmt(self: Resolver, stmt: *Stmt.If) void {
        self.resolve(stmt.condition);
        self.resolve(stmt.then_branch);
        if (stmt.else_branch) |else_branch| self.resolve(else_branch);
    }
    pub fn visitPrintStmt(self: Resolver, stmt: *Stmt.Print) void {
        self.resolve(stmt.expression);
    }
    pub fn visitReturnStmt(self: Resolver, stmt: *Stmt.Return) void {
        if (stmt.value) |value| self.resolve(value);
    }
    pub fn visitVarStmt(self: Resolver, stmt: *Stmt.Var) void {
        self.declare(stmt.name);
        self.resolve(stmt.initializer);
        self.define(stmt.name);
    }
    pub fn visitWhileStmt(self: Resolver, stmt: *Stmt.While) void {
        self.resolve(stmt.condition);
        self.resolve(stmt.body);
    }
    pub fn visitAssignExpr(self: Resolver, expr: *Expr.Assign) void {
        self.resolve(expr.value);
        self.resolveLocal(expr, expr.name);
    }
    pub fn visitBinaryExpr(self: Resolver, expr: *Expr.Binary) void {
        self.resolve(expr.left);
        self.resolve(expr.right);
    }
    pub fn visitCallExpr(self: Resolver, expr: *Expr.Call) void {
        self.resolve(expr.callee);
        for (expr.arguments) |arg| {
            self.resolve(arg);
        }
    }
    pub fn visitGroupingExpr(self: Resolver, expr: *Expr.Grouping) void {
        self.resolve(expr.expression);
    }
    pub fn visitLiteralExpr(_: Resolver, _: *Expr.Literal) void {}
    pub fn visitLogicalExpr(self: Resolver, expr: *Expr.Logical) void {
        self.resolve(expr.left);
        self.resolve(expr.right);
    }
    pub fn visitUnaryExpr(self: Resolver, expr: *Expr.Unary) void {
        self.resolve(expr.expression);
    }
    pub fn visitVariableExpr(self: Resolver, expr: *Expr.Variable) void {
        if (!(self.scopes.items.len == 0) and self.scopes.getLast().get(expr.name.lexeme).? == false) {
            const where = "";
            const resolve_error = Error{ .line = expr.name.line, .where = where, .message = "Can't read local var in its own initializer" };
            resolve_error.report();
        }
        self.resolveLocal(expr, expr.name);
    }
    fn resolveList(self: Resolver, stmts: []*Stmt) void {
        for (stmts) |stmt| {
            self.resolve(stmt);
        }
    }
    fn resolve(self: Resolver, stmt: *Stmt) void {
        stmt.accept(self);
    }
    fn beginScope(self: Resolver) void {
        const scope = std.StringHashMap(bool).init(self.allocator);
        self.scopes.append(scope);
    }
    fn endScope(self: Resolver) void {
        _ = self.scopes.pop();
    }
    fn declare(self: Resolver, name: Token) void {
        if (self.scopes.items.len == 0) return;
        const scope = self.scopes.getLast();
        scope.put(name.lexeme, false);
    }
    fn define(self: Resolver, name: Token) void {
        if (self.scopes.items.len == 0) return;
        const scope = self.scopes.getLast();
        scope.put(name.lexeme, true);
    }
    fn resolveFunction(self: Resolver, stmt: *Stmt.Function) void {
        self.beginScope();
        for (stmt.params) |param| {
            self.declare(param);
            self.define(param);
        }
        self.resolve(stmt.body);
        self.endScope();
    }
    fn resolveLocal(self: Resolver, expr: *Expr, name: Token) void {
        var i = self.scopes.items.len - 1;
        while (i >= 0) : (i -= 1) {
            if (self.scopes.items[i].get(name.lexme)) {
                self.interpreter.resolve(expr, self.scopes.items.len - 1 - i);
                return;
            }
        }
    }
};
