const std = @import("std");
const token = @import("token.zig");
pub const Expr = union(enum) {
    binary: *Binary,
    unary: *Unary,
    literal: *Literal,
    group: *Grouping,
    pub fn checkVisitorAndReturnType(comptime V: type) type {
        if (!@hasDecl(V, "ReturnType")) {
            @compileError("Visitor must have a ReturnType field");
        }

        if (@TypeOf(V.ReturnType) != type) {
            @compileError("Visitor.ReturnType must be a type");
        }
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
            const return_type = @typeInfo(@TypeOf(@field(V, method))).Fn.return_type.?;
            if (return_type != V.ReturnType) {
                @compileError(method ++ " method must return Visitor.ReturnType");
            }
        }
        return V.ReturnType;
    }

    pub fn accept(self: Expr, visitor: anytype) checkVisitorAndReturnType(@TypeOf(visitor)) {
        return switch (self) {
            .binary => |b| visitor.visitBinaryExpr(b),
            .unary => |u| visitor.visitUnaryExpr(u),
            .literal => |l| visitor.visitLiteralExpr(l),
            .group => |g| visitor.visitGroupingExpr(g),
        };
    }

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
    pub const Literal = struct {
        value: token.Literal,
        pub fn create(allocator: std.mem.Allocator, value: token.Literal) !*Expr {
            const literal = try allocator.create(Literal);
            literal.* = .{ .value = value };
            const expr = try allocator.create(Expr);
            expr.* = .{ .literal = literal };
            return expr;
        }
    };
};

pub const PrintVisitor = struct {
    pub const ReturnType = void;
    pub fn print(self: PrintVisitor, expr: *Expr) ReturnType {
        expr.accept(self);
        std.debug.print("\n", .{});
    }
    pub fn visitBinaryExpr(self: PrintVisitor, expr: *Expr.Binary) ReturnType {
        std.debug.print("({s}", .{expr.operator.lexeme});
        std.debug.print(" ", .{});
        expr.left.accept(self);
        std.debug.print(" ", .{});
        expr.right.accept(self);
        std.debug.print(")", .{});
    }
    pub fn visitGroupingExpr(self: PrintVisitor, expr: *Expr.Grouping) ReturnType {
        std.debug.print("(group ", .{});
        std.debug.print(" ", .{});
        expr.expression.accept(self);
        std.debug.print(")", .{});
    }
    pub fn visitLiteralExpr(_: PrintVisitor, expr: *Expr.Literal) ReturnType {
        const val = expr.value.toString() catch |err| std.debug.panic("{}", .{err});
        std.debug.print("{s}", .{val});
    }
    pub fn visitUnaryExpr(self: PrintVisitor, expr: *Expr.Unary) ReturnType {
        std.debug.print("({s} ", .{expr.operator.lexeme});
        expr.expression.accept(self);
        std.debug.print(")", .{});
    }
};
