const Expr = @import("expression.zig").Expr;
const Literal = @import("token.zig").Literal;
const TokenType = @import("token.zig").TokenType;
const std = @import("std");

pub const EvalVisitor = struct {
    pub const ReturnType = Literal;
    allocator: std.mem.Allocator,
    // output: *std.ArrayList(u8),
    pub fn print(self: @This(), expr: *Expr) ReturnType {
        return expr.accept(self);
    }
    pub fn visitBinaryExpr(self: @This(), expr: *Expr.Binary) ReturnType {
        const lhs = self.eval(expr.left);
        const rhs = self.eval(expr.right);
        switch (expr.operator.type) {
            TokenType.MINUS => {
                if (lhs.tagEquals(rhs)) {
                    return Literal{ .number = lhs.number - rhs.number };
                }
                std.debug.panic("Expecting numbers", .{});
            },
            TokenType.SLASH => {
                if (lhs.tagEquals(rhs)) {
                    return Literal{ .number = lhs.number / rhs.number };
                }
                std.debug.panic("Expecting numbers", .{});
            },
            TokenType.STAR => {
                if (lhs.tagEquals(rhs)) {
                    return Literal{ .number = lhs.number * rhs.number };
                }
                std.debug.panic("Expecting numbers", .{});
            },
            TokenType.PLUS => {
                if (!lhs.tagEquals(rhs)) {
                    std.debug.panic("Types dont match", .{});
                }
                if (std.meta.activeTag(lhs) == .number) {
                    return Literal{ .number = lhs.number + rhs.number };
                }
                if (std.meta.activeTag(lhs) == .string) {
                    return Literal{ .string = std.fmt.allocPrint(self.allocator, "{s}{s}", .{ lhs.string, rhs.string }) catch unreachable };
                }
                std.debug.panic("Expecting numbers/strings", .{});
            },
            TokenType.GREATER => {
                if (lhs.tagEquals(rhs)) {
                    return Literal{ .boolean = lhs.number > rhs.number };
                }
                std.debug.panic("Expecting numbers", .{});
            },
            TokenType.GREATER_EQUAL => {
                if (lhs.tagEquals(rhs)) {
                    return Literal{ .boolean = lhs.number >= rhs.number };
                }
                std.debug.panic("Expecting numbers", .{});
            },
            TokenType.LESS => {
                if (lhs.tagEquals(rhs)) {
                    return Literal{ .boolean = lhs.number < rhs.number };
                }
                std.debug.panic("Expecting numbers", .{});
            },
            TokenType.LESS_EQUAL => {
                if (lhs.tagEquals(rhs)) {
                    return Literal{ .boolean = lhs.number <= rhs.number };
                }
                std.debug.panic("Expecting numbers", .{});
            },
            TokenType.EQUAL_EQUAL => return Literal{ .boolean = isEqual(lhs, rhs) },
            TokenType.BANG_EQUAL => return Literal{ .boolean = !isEqual(lhs, rhs) },
            else => unreachable,
        }
    }
    pub fn visitGroupingExpr(self: @This(), expr: *Expr.Grouping) ReturnType {
        return self.eval(expr.expression);
    }
    pub fn visitLiteralExpr(_: @This(), expr: *Expr.Literal) ReturnType {
        return expr.value;
    }
    pub fn visitUnaryExpr(self: @This(), expr: *Expr.Unary) ReturnType {
        const rhs = self.eval(expr.expression);
        return switch (expr.operator.type) {
            TokenType.MINUS => {
                switch (rhs) {
                    .number => |n| return Literal{ .number = -n },
                    else => std.debug.panic("Expecting number", .{}),
                }
            },
            TokenType.BANG => {
                return Literal{ .boolean = !isTruthy(rhs) };
            },
            else => unreachable,
        };
    }
    fn eval(self: @This(), expr: *Expr) ReturnType {
        return expr.accept(self);
    }
    fn isEqual(u: Literal, v: Literal) bool {
        return switch (u) {
            .null => if (u.tagEquals(v)) true else false,
            .number => if (u.tagEquals(v)) u.number == v.number else std.debug.panic("Expecting numbers", .{}),
            .boolean => if (u.tagEquals(v)) u.boolean == v.boolean else std.debug.panic("Expecting booleans", .{}),
            .string => if (u.tagEquals(v)) std.mem.eql(u8, u.string, v.string) else std.debug.panic("Expecting strings", .{}),
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
