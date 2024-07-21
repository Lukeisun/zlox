const std = @import("std");
const token = @import("token.zig");
const ParsingError = error{
    NoBinaryVisitor,
    NoUnaryVisitor,
    NoGroupingVisitor,
    NoLiteralVisitor,
};
// exprs
pub const Expr = union(enum) {
    binary: *Binary,
    unary: *Unary,
    literal: *LiteralExpr,
    group: *Grouping,
    pub fn checkVisitor(comptime V: type) void {
        const required_methods = [_][]const u8{
            "visitBinaryExpr",
            "visitGroupingExpr",
            "visitLiteralExpr",
            "visitUnaryExpr",
        };

        inline for (required_methods) |method| {
            if (!@hasDecl(V, method)) {
                @compileError("Visitor is missing " ++ method ++ " method");
            }
        }
    }
    pub fn accept(self: Expr, visitor: anytype) void {
        checkVisitor(@TypeOf(visitor));
        switch (self) {
            .binary => |b| visitor.visitBinaryExpr(b),
            .unary => |u| visitor.visitUnaryExpr(u),
            .literal => |l| visitor.visitLiteralExpr(l),
            .group => |g| visitor.visitGroupingExpr(g),
        }
    }
};
pub const PrintVisitor = struct {
    pub fn print(self: PrintVisitor, expr: *Expr) void {
        expr.accept(self);
        std.debug.print("\n", .{});
    }
    pub fn visitBinaryExpr(self: PrintVisitor, expr: *Binary) void {
        std.debug.print("({s}", .{expr.operator.lexeme});
        std.debug.print(" ", .{});
        expr.left.accept(self);
        std.debug.print(" ", .{});
        expr.right.accept(self);
        std.debug.print(")", .{});
    }
    pub fn visitGroupingExpr(self: PrintVisitor, expr: *Grouping) void {
        std.debug.print("(group ", .{});
        std.debug.print(" ", .{});
        expr.expression.accept(self);
        std.debug.print(")", .{});
    }
    pub fn visitLiteralExpr(_: PrintVisitor, expr: *LiteralExpr) void {
        const val = expr.value.toString() catch |err| std.debug.panic("{}", .{err});
        std.debug.print("{s}", .{val});
    }
    pub fn visitUnaryExpr(self: PrintVisitor, expr: *Unary) void {
        std.debug.print("({s} ", .{expr.operator.lexeme});
        expr.expression.accept(self);
        std.debug.print(")", .{});
    }
};
pub const Binary = struct {
    left: *Expr,
    operator: token.Token,
    right: *Expr,
    pub fn create(allocator: std.mem.Allocator, left: *Expr, operator: token.Token, right: *Expr) !*Expr {
        const binary = try allocator.create(Binary);
        binary.* = .{ .left = left, .operator = operator, .right = right };
        const expr = try allocator.create(Expr);
        expr.* = .{ .binary = binary };
        return expr;
    }
};
pub const Grouping = struct {
    expression: *Expr,
    pub fn create(allocator: std.mem.Allocator, expression: *Expr) !*Expr {
        const group = try allocator.create(Grouping);
        group.* = .{ .expression = expression };
        const expr = try allocator.create(Expr);
        expr.* = .{ .group = group };
        return expr;
    }
};
pub const Unary = struct {
    operator: token.Token,
    expression: *Expr,
    pub fn create(allocator: std.mem.Allocator, expression: *Expr, operator: token.Token) !*Expr {
        const unary = try allocator.create(Unary);
        unary.* = .{ .expression = expression, .operator = operator };
        const expr = try allocator.create(Expr);
        expr.* = .{ .unary = unary };
        return expr;
    }
};
pub const LiteralExpr = struct {
    value: token.Literal,
    pub fn create(allocator: std.mem.Allocator, value: token.Literal) !*Expr {
        const literal = try allocator.create(LiteralExpr);
        literal.* = .{ .value = value };
        const expr = try allocator.create(Expr);
        expr.* = .{ .literal = literal };
        return expr;
    }
};
