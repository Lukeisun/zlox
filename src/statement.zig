const std = @import("std");
const Token = @import("token.zig").Token;
const Expr = @import("expression.zig").Expr;
pub const Stmt = union(enum) {
    expression: *Expression,
    variable: *Var,
    print: *Print,
    pub fn create(allocator: std.mem.Allocator, stmt_data: anytype) !*Stmt {
        const stmt = try allocator.create(Stmt);
        stmt.* = stmt_data;
        return stmt;
    }
    pub fn accept(self: *Stmt, visitor: anytype) !void {
        return switch (self.*) {
            .expression => |e| try visitor.visitExpressionStmt(e),
            .print => |p| try visitor.visitPrintStmt(p),
            .variable => |v| try visitor.visitVarStmt(v),
        };
    }
    pub const Expression = struct {
        expression: *Expr,
        pub fn create(allocator: std.mem.Allocator, expression: *Expr) !*Stmt {
            const expression_struct = try allocator.create(Expression);
            expression_struct.* = .{ .expression = expression };
            return try Stmt.create(allocator, .{ .expression = expression_struct });
        }
    };
    pub const Print = struct {
        expression: *Expr,
        pub fn create(allocator: std.mem.Allocator, expression: *Expr) !*Stmt {
            const print = try allocator.create(Print);
            print.* = .{ .expression = expression };
            return try Stmt.create(allocator, .{ .print = print });
        }
    };
    pub const Var = struct {
        name: Token,
        initializer: *Expr,
        pub fn create(allocator: std.mem.Allocator, name: Token, initializer: *Expr) !*Stmt {
            const variable = try allocator.create(Var);
            variable.* = .{ .initializer = initializer, .name = name };
            return try Stmt.create(allocator, .{ .variable = variable });
        }
    };
};
