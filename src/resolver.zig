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

const FunctionType = enum {
    NONE,
    FUNCTION,
    METHOD,
};
// Really need to review how this actually works,
// I sort of get it? but not really
pub const Resolver = struct {
    pub const ExprReturnType = void;
    interpreter: *Interpreter,
    scopes: std.ArrayList(*std.StringHashMap(bool)),
    currentFunction: FunctionType = FunctionType.NONE,
    allocator: std.mem.Allocator,
    had_error: bool = false,
    pub fn create(allocator: std.mem.Allocator, interpreter: *Interpreter) Resolver {
        const scopes = std.ArrayList(*std.StringHashMap(bool)).init(allocator);
        return .{
            .allocator = allocator,
            .interpreter = interpreter,
            .scopes = scopes,
        };
    }
    pub fn visitBlock(self: *Resolver, stmt: *Stmt.Block) !void {
        self.beginScope();
        self.resolveList(stmt.statements);
        self.endScope();
    }
    pub fn visitExpressionStmt(self: *Resolver, stmt: *Stmt.Expression) !void {
        self.resolveExpr(stmt.expression);
    }
    pub fn visitFunctionStmt(self: *Resolver, stmt: *Stmt.Function) !void {
        self.declare(stmt.name);
        self.define(stmt.name);
        self.resolveFunction(stmt, FunctionType.FUNCTION);
    }
    pub fn visitIfStmt(self: *Resolver, stmt: *Stmt.If) !void {
        self.resolveExpr(stmt.condition);
        self.resolve(stmt.then_branch);
        if (stmt.else_branch) |else_branch| self.resolve(else_branch);
    }
    pub fn visitPrintStmt(self: *Resolver, stmt: *Stmt.Print) !void {
        self.resolveExpr(stmt.expression);
    }
    pub fn visitReturnStmt(self: *Resolver, stmt: *Stmt.Return) !void {
        if (self.currentFunction == .NONE) {
            self.had_error = true;
            const where = "";
            const resolve_error = Error{ .line = stmt.keyword.line, .where = where, .message = "Can't return from top level code" };
            resolve_error.report();
        }
        if (stmt.value) |value| self.resolveExpr(value);
    }
    pub fn visitVarStmt(self: *Resolver, stmt: *Stmt.Var) !void {
        self.declare(stmt.name);
        self.resolveExpr(stmt.initializer);
        self.define(stmt.name);
    }
    pub fn visitClassStmt(self: *Resolver, stmt: *Stmt.Class) !void {
        self.declare(stmt.name);
        for (stmt.methods) |method| {
            switch (method.*) {
                .function => |f| self.resolveFunction(f, FunctionType.METHOD),
                inline else => unreachable,
            }
        }
        self.define(stmt.name);
    }
    pub fn visitWhileStmt(self: *Resolver, stmt: *Stmt.While) !void {
        self.resolveExpr(stmt.condition);
        self.resolve(stmt.body);
    }
    pub fn visitAssignExpr(self: *Resolver, expr: *Expr.Assign) void {
        self.resolveExpr(expr.value);
        self.resolveLocal(Expr{ .assign = expr }, expr.name);
    }
    pub fn visitBinaryExpr(self: *Resolver, expr: *Expr.Binary) void {
        self.resolveExpr(expr.left);
        self.resolveExpr(expr.right);
    }
    pub fn visitCallExpr(self: *Resolver, expr: *Expr.Call) void {
        self.resolveExpr(expr.callee);
        for (expr.arguments) |arg| {
            self.resolveExpr(arg);
        }
    }
    pub fn visitGetExpr(self: *Resolver, expr: *Expr.Get) void {
        self.resolveExpr(expr.object);
    }
    pub fn visitGroupingExpr(self: *Resolver, expr: *Expr.Grouping) void {
        self.resolveExpr(expr.expression);
    }
    pub fn visitLiteralExpr(_: *Resolver, _: *Expr.Literal) void {}
    pub fn visitLogicalExpr(self: *Resolver, expr: *Expr.Logical) void {
        self.resolveExpr(expr.left);
        self.resolveExpr(expr.right);
    }
    pub fn visitSetExpr(self: *Resolver, expr: *Expr.Set) void {
        self.resolveExpr(expr.object);
        self.resolveExpr(expr.value);
    }
    pub fn visitUnaryExpr(self: *Resolver, expr: *Expr.Unary) void {
        self.resolveExpr(expr.expression);
    }
    pub fn visitVariableExpr(self: *Resolver, expr: *Expr.Variable) void {
        if (self.scopes.items.len != 0) {
            const b = (self.scopes.getLast().get(expr.name.lexeme)) orelse true;
            if (!b) {
                self.had_error = true;
                const where = "";
                const resolve_error = Error{ .line = expr.name.line, .where = where, .message = "Can't read local var in its own initializer" };
                resolve_error.report();
            }
        }
        self.resolveLocal(Expr{ .variable = expr }, expr.name);
    }
    pub fn resolveList(self: *Resolver, stmts: []*Stmt) void {
        for (stmts) |stmt| {
            self.resolve(stmt);
        }
    }
    pub fn resolve(self: *Resolver, stmt: *Stmt) void {
        stmt.accept(self) catch unreachable;
    }
    pub fn resolveExpr(self: *Resolver, expr: *Expr) void {
        expr.accept(self);
    }
    fn beginScope(self: *Resolver) void {
        const scope = self.allocator.create(std.StringHashMap(bool)) catch {
            std.debug.panic("OOM", .{});
        };
        scope.* = std.StringHashMap(bool).init(self.allocator);
        self.scopes.append(scope) catch {
            std.debug.panic("OOM", .{});
        };
    }
    fn endScope(self: *Resolver) void {
        _ = self.scopes.pop();
    }
    fn declare(self: *Resolver, name: Token) void {
        if (self.scopes.items.len == 0) return;
        var scope = self.scopes.getLast();
        if (scope.get(name.lexeme)) |_| {
            self.had_error = true;
            const where = "";
            const resolve_error = Error{ .line = name.line, .where = where, .message = "Already a varaible with this name in scope" };
            resolve_error.report();
        }
        scope.put(name.lexeme, false) catch {
            std.debug.panic("OOM", .{});
        };
    }
    fn define(self: *Resolver, name: Token) void {
        if (self.scopes.items.len == 0) return;
        var scope = self.scopes.getLast();
        scope.put(name.lexeme, true) catch {
            std.debug.panic("OOM", .{});
        };
    }
    fn resolveFunction(self: *Resolver, stmt: *Stmt.Function, function_type: FunctionType) void {
        const enclosingFunction = self.currentFunction;
        self.currentFunction = function_type;
        self.beginScope();
        for (stmt.params) |param| {
            self.declare(param);
            self.define(param);
        }
        self.resolveList(stmt.body);
        self.endScope();
        self.currentFunction = enclosingFunction;
    }
    fn resolveLocal(self: *Resolver, expr: Expr, name: Token) void {
        if (self.scopes.items.len == 0) return;
        var i: usize = self.scopes.items.len;
        while (i > 0) {
            i -= 1;
            const b = self.scopes.items[i].get(name.lexeme) orelse false;
            if (b) {
                self.interpreter.resolve(expr, self.scopes.items.len - 1 - i);
                return;
            }
        }
    }
};
