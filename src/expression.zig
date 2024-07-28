const std = @import("std");
const token = @import("token.zig");
pub const Expr = union(enum) {
    binary: *Binary,
    unary: *Unary,
    literal: *Literal,
    group: *Grouping,
    pub fn checkVisitorAndReturnType(comptime V: anytype) type {
        if (@typeInfo(V) != .Struct) {
            @compileError("Expecting struct");
        }
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
    // for non arena uses
    pub fn destruct(self: *Expr, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .binary => |b| {
                b.left.destruct(allocator);
                b.right.destruct(allocator);
                allocator.destroy(b);
            },
            .unary => |u| {
                u.expression.destruct(allocator);
                allocator.destroy(u);
            },
            .literal => |l| {
                allocator.destroy(l);
            },
            .group => |g| {
                g.expression.destruct(allocator);
                allocator.destroy(g);
            },
        }
        allocator.destroy(self);
    }

    pub fn accept(self: *Expr, visitor: anytype) checkVisitorAndReturnType(@TypeOf(visitor.*)) {
        return switch (self.*) {
            .binary => |b| visitor.visitBinaryExpr(b),
            .unary => |u| visitor.visitUnaryExpr(u),
            .literal => |l| visitor.visitLiteralExpr(l),
            .group => |g| visitor.visitGroupingExpr(g),
        };
    }
    pub fn create(allocator: std.mem.Allocator, expr_data: anytype) !*Expr {
        // Not really sure if I should do much handling here, I had some but deleted cause
        // noticed I got compiler errors anyhow (if i called create with invalid expr_data)
        const expr = try allocator.create(Expr);
        expr.* = expr_data;
        return expr;
    }

    pub const Binary = struct {
        left: *Expr,
        operator: token.Token,
        right: *Expr,
        pub fn create(allocator: std.mem.Allocator, left: *Expr, operator: token.Token, right: *Expr) !*Expr {
            const binary = try allocator.create(Binary);
            binary.* = .{ .left = left, .operator = operator, .right = right };
            return try Expr.create(allocator, .{ .binary = binary });
        }
    };
    pub const Grouping = struct {
        expression: *Expr,
        pub fn create(allocator: std.mem.Allocator, expression: *Expr) !*Expr {
            const group = try allocator.create(Grouping);
            group.* = .{ .expression = expression };
            return try Expr.create(allocator, .{ .group = group });
        }
    };
    pub const Unary = struct {
        operator: token.Token,
        expression: *Expr,
        pub fn create(allocator: std.mem.Allocator, operator: token.Token, expression: *Expr) !*Expr {
            const unary = try allocator.create(Unary);
            unary.* = .{ .expression = expression, .operator = operator };
            return try Expr.create(allocator, .{ .unary = unary });
        }
    };
    pub const Literal = struct {
        value: token.Literal,
        pub fn create(allocator: std.mem.Allocator, value: token.Literal) !*Expr {
            const literal = try allocator.create(Literal);
            literal.* = .{ .value = value };
            return try Expr.create(allocator, .{ .literal = literal });
        }
    };
};
pub fn panic(err: anyerror) void {
    std.debug.panic("Error {s}", .{@errorName(err)});
}

pub const PrintVisitor = struct {
    pub const ReturnType = void;
    output: *std.ArrayList(u8),
    pub fn create(output: *std.ArrayList(u8)) PrintVisitor {
        return PrintVisitor{ .output = output };
    }
    pub fn print(self: PrintVisitor, expr: *Expr) ReturnType {
        expr.accept(self);
    }
    pub fn visitBinaryExpr(self: PrintVisitor, expr: *Expr.Binary) ReturnType {
        std.fmt.format(self.output.writer(), "({s} ", .{expr.operator.lexeme}) catch |err| panic(err);
        expr.left.accept(self);
        std.fmt.format(self.output.writer(), " ", .{}) catch |err| panic(err);
        expr.right.accept(self);
        std.fmt.format(self.output.writer(), ")", .{}) catch |err| panic(err);
    }
    pub fn visitGroupingExpr(self: PrintVisitor, expr: *Expr.Grouping) ReturnType {
        std.fmt.format(self.output.writer(), "(group ", .{}) catch |err| panic(err);
        expr.expression.accept(self);
        std.fmt.format(self.output.writer(), ")", .{}) catch |err| panic(err);
    }
    pub fn visitLiteralExpr(self: PrintVisitor, expr: *Expr.Literal) ReturnType {
        var buf: [128]u8 = undefined;
        if (expr.value.toString(&buf)) |val| {
            std.fmt.format(self.output.writer(), "{s}", .{val}) catch |err| panic(err);
        } else |err| {
            panic(err);
        }
    }
    pub fn visitUnaryExpr(self: PrintVisitor, expr: *Expr.Unary) ReturnType {
        std.fmt.format(self.output.writer(), "({s} ", .{expr.operator.lexeme}) catch |err| panic(err);
        expr.expression.accept(self);
        std.fmt.format(self.output.writer(), ")", .{}) catch |err| panic(err);
    }
};
