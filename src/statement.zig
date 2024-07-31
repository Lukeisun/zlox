const std = @import("std");
const Token = @import("token.zig").Token;
const Expr = @import("expression.zig").Expr;
pub const Stmt = union(enum) {
    expression: Expression,
    variable: Var,
    print: Print,
    block: Block,
    pub fn accept(self: Stmt, visitor: anytype) !void {
        return switch (self) {
            .expression => |e| try visitor.visitExpressionStmt(e),
            .print => |p| try visitor.visitPrintStmt(p),
            .variable => |v| try visitor.visitVarStmt(v),
            .block => |b| try visitor.visitBlock(b),
        };
    }
    pub const Expression = struct {
        expression: *Expr,
        pub fn create(expression: *Expr) !Stmt {
            return Stmt{ .expression = Expression{ .expression = expression } };
        }
    };
    pub const Print = struct {
        expression: *Expr,
        pub fn create(expression: *Expr) !Stmt {
            return Stmt{ .print = .{ .expression = expression } };
        }
    };
    pub const Var = struct {
        name: Token,
        initializer: *Expr,
        pub fn create(name: Token, initializer: *Expr) !Stmt {
            return Stmt{ .variable = .{ .name = name, .initializer = initializer } };
        }
    };
    pub const Block = struct {
        statements: []Stmt,
        pub fn create(statements: []Stmt) !Stmt {
            return Stmt{ .block = .{ .statements = statements } };
        }
    };
};
