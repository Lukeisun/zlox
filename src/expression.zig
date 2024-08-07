const std = @import("std");
const Token = @import("token.zig").Token;
const token = @import("token.zig");
pub const Expr = union(enum) {
    binary: *Binary,
    unary: *Unary,
    literal: *Literal,
    group: *Grouping,
    variable: *Variable,
    assign: *Assign,
    logical: *Logical,
    call: *Call,
    get: *Get,
    set: *Set,
    this: *This,
    super: *Super,
    // Don't need this since the zig compiler checks this stuff anyhow.
    // But leaving it in for uh, memories!
    pub fn checkVisitorAndExprReturnType(comptime V: anytype) type {
        if (@typeInfo(V) != .Struct) {
            @compileError("Expecting struct");
        }
        if (!@hasDecl(V, "ExprReturnType")) {
            @compileError("Visitor must have a ExprReturnType field");
        }

        if (@TypeOf(V.ExprReturnType) != type) {
            @compileError("Visitor.ExprReturnType must be a type");
        }
        const required_methods = [_][]const u8{
            "visitBinaryExpr",
            "visitGroupingExpr",
            "visitLiteralExpr",
            "visitUnaryExpr",
            "visitVariableExpr",
            "visitAssignExpr",
            "visitLogicalExpr",
            "visitCallExpr",
            "visitGetExpr",
        };

        inline for (required_methods) |method| {
            if (!@hasDecl(V, method)) {
                @compileError("Visitor is missing " ++ method ++ " method");
            }
            const return_type = @typeInfo(@TypeOf(@field(V, method))).Fn.return_type.?;
            if (return_type != V.ExprReturnType) {
                @compileError(method ++ " method must return Visitor.ExprReturnType");
            }
        }
        return V.ExprReturnType;
    }

    pub fn accept(self: *Expr, visitor: anytype) checkVisitorAndExprReturnType(@TypeOf(visitor.*)) {
        return switch (self.*) {
            .binary => |b| visitor.visitBinaryExpr(b),
            .unary => |u| visitor.visitUnaryExpr(u),
            .literal => |l| visitor.visitLiteralExpr(l),
            .group => |g| visitor.visitGroupingExpr(g),
            .variable => |v| visitor.visitVariableExpr(v),
            .assign => |a| visitor.visitAssignExpr(a),
            .logical => |l| visitor.visitLogicalExpr(l),
            .call => |l| visitor.visitCallExpr(l),
            .get => |g| visitor.visitGetExpr(g),
            .set => |s| visitor.visitSetExpr(s),
            .this => |t| visitor.visitThisExpr(t),
            .super => |s| visitor.visitSuperExpr(s),
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
        operator: Token,
        right: *Expr,
        pub fn create(allocator: std.mem.Allocator, left: *Expr, operator: Token, right: *Expr) !*Expr {
            const binary = try allocator.create(Binary);
            binary.* = .{ .left = left, .operator = operator, .right = right };
            return Expr.create(allocator, .{ .binary = binary });
        }
    };
    pub const Grouping = struct {
        expression: *Expr,
        pub fn create(allocator: std.mem.Allocator, expression: *Expr) !*Expr {
            const group = try allocator.create(Grouping);
            group.* = .{ .expression = expression };
            return Expr.create(allocator, .{ .group = group });
        }
    };
    pub const Unary = struct {
        operator: Token,
        expression: *Expr,
        pub fn create(allocator: std.mem.Allocator, operator: Token, expression: *Expr) !*Expr {
            const unary = try allocator.create(Unary);
            unary.* = .{ .expression = expression, .operator = operator };
            return Expr.create(allocator, .{ .unary = unary });
        }
    };
    pub const Literal = struct {
        value: token.Literal,
        pub fn create(allocator: std.mem.Allocator, value: token.Literal) !*Expr {
            const literal = try allocator.create(Literal);
            literal.* = .{ .value = value };
            return Expr.create(allocator, .{ .literal = literal });
        }
    };
    pub const Variable = struct {
        name: Token,
        pub fn create(allocator: std.mem.Allocator, name: Token) !*Expr {
            const variable = try allocator.create(Variable);
            variable.* = .{ .name = name };
            return Expr.create(allocator, .{ .variable = variable });
        }
    };
    pub const Assign = struct {
        name: Token,
        value: *Expr,
        pub fn create(allocator: std.mem.Allocator, name: Token, value: *Expr) !*Expr {
            const assign = try allocator.create(Assign);
            assign.* = .{ .name = name, .value = value };
            return Expr.create(allocator, .{ .assign = assign });
        }
    };
    pub const Logical = struct {
        left: *Expr,
        operator: Token,
        right: *Expr,
        pub fn create(allocator: std.mem.Allocator, left: *Expr, operator: Token, right: *Expr) !*Expr {
            const logical = try allocator.create(Logical);
            logical.* = .{ .left = left, .operator = operator, .right = right };
            return Expr.create(allocator, .{ .logical = logical });
        }
    };
    pub const Call = struct {
        callee: *Expr,
        paren: Token,
        arguments: []*Expr,
        pub fn create(allocator: std.mem.Allocator, callee: *Expr, paren: Token, arguments: []*Expr) !*Expr {
            const call = try allocator.create(Call);
            call.* = .{ .callee = callee, .paren = paren, .arguments = arguments };
            return Expr.create(allocator, .{ .call = call });
        }
    };
    pub const Get = struct {
        object: *Expr,
        name: Token,
        pub fn create(allocator: std.mem.Allocator, object: *Expr, name: Token) !*Expr {
            const get = try allocator.create(Get);
            get.* = .{ .object = object, .name = name };
            return Expr.create(allocator, .{ .get = get });
        }
    };
    pub const Set = struct {
        object: *Expr,
        name: Token,
        value: *Expr,
        pub fn create(allocator: std.mem.Allocator, object: *Expr, name: Token, value: *Expr) !*Expr {
            const set = try allocator.create(Set);
            set.* = .{ .object = object, .name = name, .value = value };
            return Expr.create(allocator, .{ .set = set });
        }
    };
    pub const This = struct {
        keyword: Token,
        pub fn create(allocator: std.mem.Allocator, keyword: Token) !*Expr {
            const this = try allocator.create(This);
            this.* = .{ .keyword = keyword };
            return Expr.create(allocator, .{ .this = this });
        }
    };
    pub const Super = struct {
        keyword: Token,
        method: Token,
        pub fn create(allocator: std.mem.Allocator, keyword: Token, method: Token) !*Expr {
            const super = try allocator.create(Super);
            super.* = .{ .keyword = keyword, .method = method };
            return Expr.create(allocator, .{ .super = super });
        }
    };
};
pub fn panic(err: anyerror) void {
    std.debug.panic("Error {s}", .{@errorName(err)});
}
