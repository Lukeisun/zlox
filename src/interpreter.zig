const std = @import("std");
const Expr = @import("expression.zig").Expr;
const Literal = @import("token.zig").Literal;
const TokenType = @import("token.zig").TokenType;
const Token = @import("token.zig").Token;
const Stmt = @import("statement.zig").Stmt;
const Environment = @import("environment.zig").Environment;
const LoxFunction = @import("callable.zig").LoxFunction;
const RuntimeError = @import("error.zig").RuntimeError;
const Callable = @import("callable.zig").Callable;
const Clock = @import("callable.zig").Clock;

pub const Interpreter = struct {
    pub const ExprReturnType = RuntimeError!Literal;
    repl: bool = false,
    allocator: std.mem.Allocator,
    environment: *Environment,
    globals: *Environment,
    // probably split this up into a differrent struct?
    had_runtime_error: bool,
    run_time_offender: ?Token,
    return_value: ?Literal = null,

    pub fn create(allocator: std.mem.Allocator) Interpreter {
        var globals = Environment.create(allocator);
        const z = Clock{};
        const clock = Callable.create(allocator, .{ .clock = z }) catch {
            std.debug.panic("OOM\n", .{});
        };
        globals.define("clock", Literal{ .callable = clock });
        return Interpreter{
            .allocator = allocator,
            .had_runtime_error = false,
            .run_time_offender = null,
            .environment = globals,
            .globals = globals,
        };
    }

    pub fn interpret(self: *@This(), statements: []*Stmt) !void {
        for (statements) |statement| {
            self.execute(statement) catch |err| {
                switch (err) {
                    error.OutOfMemory => std.debug.panic("OOM", .{}),
                    else => {
                        const t = self.run_time_offender orelse unreachable;
                        std.log.err("[line: {d}] Offending Token: {s}\n\t{s}", .{
                            t.line,
                            t.lexeme,
                            @errorName(err),
                        });
                        return err;
                    },
                }
            };
        }
        std.debug.assert(self.return_value == null);
    }
    pub fn visitBinaryExpr(self: *@This(), expr: *Expr.Binary) ExprReturnType {
        const lhs = try self.eval(expr.left);
        const rhs = try self.eval(expr.right);
        var rt_error: RuntimeError = undefined;
        switch (expr.operator.type) {
            TokenType.MINUS => {
                if (lhs.tagEquals(rhs) and std.meta.activeTag(lhs) == .number) {
                    return Literal{ .number = lhs.number - rhs.number };
                }
                rt_error = RuntimeError.ExpectingNumbers;
            },
            TokenType.SLASH => {
                if (lhs.tagEquals(rhs) and std.meta.activeTag(lhs) == .number) {
                    if (rhs.number == 0) return self.setLoxError(RuntimeError.DivisionByZero, expr.operator);
                    return Literal{ .number = lhs.number / rhs.number };
                }
                rt_error = RuntimeError.ExpectingNumbers;
            },
            TokenType.STAR => {
                if (lhs.tagEquals(rhs) and std.meta.activeTag(lhs) == .number) {
                    return Literal{ .number = lhs.number * rhs.number };
                }
                rt_error = RuntimeError.ExpectingNumbers;
            },
            TokenType.PLUS => {
                if (!lhs.tagEquals(rhs)) {
                    rt_error = RuntimeError.NonMatchingTypes;
                } else {
                    if (std.meta.activeTag(lhs) == .number) {
                        return Literal{ .number = lhs.number + rhs.number };
                    }
                    if (std.meta.activeTag(lhs) == .string) {
                        return Literal{ .string = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ lhs.string, rhs.string }) };
                    }
                    rt_error = RuntimeError.ExpectingNumbersOrStrings;
                }
            },
            TokenType.GREATER => {
                if (lhs.tagEquals(rhs)) {
                    return Literal{ .boolean = lhs.number > rhs.number };
                }
                rt_error = RuntimeError.ExpectingNumbers;
            },
            TokenType.GREATER_EQUAL => {
                if (lhs.tagEquals(rhs)) {
                    return Literal{ .boolean = lhs.number >= rhs.number };
                }
                rt_error = RuntimeError.ExpectingNumbers;
            },
            TokenType.LESS => {
                if (lhs.tagEquals(rhs)) {
                    return Literal{ .boolean = lhs.number < rhs.number };
                }
                rt_error = RuntimeError.ExpectingNumbers;
            },
            TokenType.LESS_EQUAL => {
                if (lhs.tagEquals(rhs)) {
                    return Literal{ .boolean = lhs.number <= rhs.number };
                }
                rt_error = RuntimeError.ExpectingNumbers;
            },
            TokenType.EQUAL_EQUAL => return Literal{ .boolean = try isEqual(lhs, rhs) },
            TokenType.BANG_EQUAL => return Literal{ .boolean = !(try isEqual(lhs, rhs)) },
            else => unreachable,
        }
        return self.setLoxError(rt_error, expr.operator);
    }
    pub fn visitGroupingExpr(self: *@This(), expr: *Expr.Grouping) ExprReturnType {
        return self.eval(expr.expression);
    }
    pub fn visitLiteralExpr(_: *@This(), expr: *Expr.Literal) ExprReturnType {
        return expr.value;
    }
    pub fn visitLogicalExpr(self: *@This(), expr: *Expr.Logical) ExprReturnType {
        const left = try self.eval(expr.left);
        if (expr.operator.type == TokenType.OR) {
            if (isTruthy(left)) return left;
        } else {
            if (!isTruthy(left)) return left;
        }
        return self.eval(expr.right);
    }
    pub fn visitUnaryExpr(self: *@This(), expr: *Expr.Unary) ExprReturnType {
        const rhs = try self.eval(expr.expression);
        var rt_error: RuntimeError = undefined;
        switch (expr.operator.type) {
            TokenType.MINUS => {
                switch (rhs) {
                    .number => |n| return Literal{ .number = -n },
                    else => rt_error = RuntimeError.ExpectingNumber,
                }
            },
            TokenType.BANG => {
                return Literal{ .boolean = !isTruthy(rhs) };
            },
            else => unreachable,
        }
        return self.setLoxError(RuntimeError.ExpectingNumber, expr.operator);
    }
    pub fn visitVariableExpr(self: *@This(), expr: *Expr.Variable) ExprReturnType {
        return self.environment.get(expr.name) catch |err| {
            return self.setLoxError(err, expr.name);
        };
    }
    pub fn visitAssignExpr(self: *@This(), expr: *Expr.Assign) ExprReturnType {
        const value = try self.eval(expr.value);
        self.environment.assign(expr.name, value) catch |err| {
            return self.setLoxError(err, expr.name);
        };
        return value;
    }
    pub fn visitCallExpr(self: *@This(), expr: *Expr.Call) ExprReturnType {
        var callee = try self.eval(expr.callee);
        var arguments = std.ArrayList(Literal).init(self.allocator);
        for (expr.arguments) |arg| {
            const res = try self.eval(arg);
            try arguments.append(res);
        }
        if (std.meta.activeTag(callee) != .callable) {
            return self.setLoxError(RuntimeError.NotFnOrClass, expr.paren);
        }
        if (arguments.items.len != callee.callable.arity()) {
            std.log.err("Expected {d} arguments but got {d}", .{ callee.callable.arity(), arguments.items.len });
            return self.setLoxError(RuntimeError.MismatchedArity, expr.paren);
        }
        const slice = try arguments.toOwnedSlice();
        return callee.callable.call(self, slice);
    }
    fn eval(self: *@This(), expr: *Expr) ExprReturnType {
        return expr.accept(self);
    }
    fn execute(self: *@This(), stmt: *Stmt) !void {
        try stmt.accept(self);
    }
    pub fn executeBlock(self: *@This(), statements: []*Stmt, env: *Environment) !void {
        const previous = self.environment;
        errdefer self.environment = previous;
        defer self.environment = previous;
        self.environment = env;
        for (statements) |statement| {
            try self.execute(statement);
        }
    }
    pub fn visitBlock(self: *@This(), stmt: *Stmt.Block) RuntimeError!void {
        const env = Environment.createWithEnv(self.allocator, self.environment);
        try self.executeBlock(stmt.statements, env);
    }
    pub fn visitExpressionStmt(self: *@This(), stmt: *Stmt.Expression) RuntimeError!void {
        const val = try self.eval(stmt.expression);
        if (self.repl) {
            self.print(val);
        }
    }
    pub fn visitFunctionStmt(self: *@This(), stmt: *Stmt.Function) RuntimeError!void {
        const fun = try LoxFunction.create(self.allocator, stmt);
        const literal = Literal{ .callable = try fun.callable() };
        self.environment.define(stmt.name.lexeme, literal);
    }
    pub fn visitIfStmt(self: *@This(), stmt: *Stmt.If) RuntimeError!void {
        const condition = try self.eval(stmt.condition);
        if (isTruthy(condition)) {
            try self.execute(stmt.then_branch);
        } else if (stmt.else_branch) |else_branch| {
            try self.execute(else_branch);
        }
    }
    pub fn visitPrintStmt(self: *@This(), stmt: *Stmt.Print) RuntimeError!void {
        const val = try self.eval(stmt.expression);
        self.print(val);
    }
    pub fn visitReturnStmt(self: *@This(), stmt: *Stmt.Return) RuntimeError!void {
        var value = Literal{ .null = {} };
        if (stmt.value) |val| value = try self.eval(val);
        self.return_value = value;
        return RuntimeError.Return;
    }
    pub fn visitVarStmt(self: *@This(), stmt: *Stmt.Var) RuntimeError!void {
        const value = try self.eval(stmt.initializer);
        self.environment.define(stmt.name.lexeme, value);
    }
    pub fn visitWhileStmt(self: *@This(), stmt: *Stmt.While) RuntimeError!void {
        while (isTruthy(try self.eval(stmt.condition))) {
            try self.execute(stmt.body);
        }
    }
    fn setLoxError(self: *@This(), err: RuntimeError, offender: Token) RuntimeError {
        self.run_time_offender = offender;
        self.had_runtime_error = true;
        return err;
    }
    fn isEqual(u: Literal, v: Literal) !bool {
        return switch (u) {
            .null => if (u.tagEquals(v)) true else false,
            .number => if (u.tagEquals(v)) u.number == v.number else return RuntimeError.ExpectingNumbers,
            .boolean => if (u.tagEquals(v)) u.boolean == v.boolean else return RuntimeError.ExpectingBooleans,
            .string => if (u.tagEquals(v)) std.mem.eql(u8, u.string, v.string) else return RuntimeError.ExpectingStrings,
            // TODO: fix
            .callable => unreachable,
        };
    }
    fn isTruthy(literal: Literal) bool {
        switch (literal) {
            .boolean => |b| return b,
            .null => return false,
            else => return true,
        }
    }
    fn print(self: *@This(), value: Literal) void {
        const str = value.toStringAlloc(self.allocator) catch {
            std.debug.panic("Buffer not big enough\n", .{});
        };
        const stdout = std.io.getStdOut();
        stdout.writeAll(str) catch |err| {
            std.debug.panic("Error writing to stdout - {s}\n", .{@errorName(err)});
        };
        stdout.writeAll("\n") catch |err| {
            std.debug.panic("Error writing to stdout - {s}\n", .{@errorName(err)});
        };
    }
};
