const Expr = @import("expression.zig").Expr;
const Literal = @import("token.zig").Literal;
const TokenType = @import("token.zig").TokenType;
const Token = @import("token.zig").Token;
const Stmt = @import("statement.zig").Stmt;
const std = @import("std");
const RuntimeError = error{
    ExpectingNumbers,
    ExpectingNumber,
    NonMatchingTypes,
    ExpectingNumbersOrStrings,
    ExpectingBooleans,
    ExpectingStrings,
    DivisionByZero,
} || std.fmt.AllocPrintError;

pub const EvalVisitor = struct {
    pub const ExprReturnType = RuntimeError!Literal;
    allocator: std.mem.Allocator,
    // probably split this up into a differrent struct?
    had_runtime_error: bool,
    run_time_offender: ?Token,
    pub fn create(allocator: std.mem.Allocator) EvalVisitor {
        return EvalVisitor{ .allocator = allocator, .had_runtime_error = false, .run_time_offender = null };
    }
    pub fn interpret(self: *@This(), statements: *std.ArrayList(*Stmt)) !void {
        for (statements.*.items) |statement| {
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
    fn eval(self: *@This(), expr: *Expr) ExprReturnType {
        return expr.accept(self);
    }
    fn execute(self: *@This(), stmt: *Stmt) !void {
        try stmt.accept(self);
    }
    pub fn visitExpressionStmt(self: *@This(), stmt: *Stmt.Expression) !void {
        _ = try self.eval(stmt.expression);
    }
    pub fn visitPrintStmt(self: *@This(), stmt: *Stmt.Print) !void {
        const val = try self.eval(stmt.expression);
        var buf: [128]u8 = undefined;
        const str = try val.toString(&buf);
        const stdout = std.io.getStdOut();
        try stdout.writeAll(str);
        try stdout.writeAll("\n");
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
        };
    }
    fn isTruthy(literal: Literal) bool {
        switch (literal) {
            .boolean => |b| return b,
            .null => return false,
            else => return true,
        }
    }
};
