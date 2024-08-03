const std = @import("std");
const Token = @import("token.zig").Token;
const Expr = @import("expression.zig").Expr;
pub const Stmt = union(enum) {
    expression: *Expression,
    variable: *Var,
    print: *Print,
    block: *Block,
    if_stmt: *If,
    while_stmt: *While,
    function: *Function,
    pub fn accept(self: Stmt, visitor: anytype) !void {
        return switch (self) {
            .expression => |e| try visitor.visitExpressionStmt(e),
            .print => |p| try visitor.visitPrintStmt(p),
            .variable => |v| try visitor.visitVarStmt(v),
            .block => |b| try visitor.visitBlock(b),
            .if_stmt => |i| try visitor.visitIfStmt(i),
            .while_stmt => |w| try visitor.visitWhileStmt(w),
            .function => |f| try visitor.visitFunctionStmt(f),
        };
    }
    pub fn create(allocator: std.mem.Allocator, stmt_data: anytype) !*Stmt {
        const stmt = try allocator.create(Stmt);
        stmt.* = stmt_data;
        return stmt;
    }
    pub const Expression = struct {
        expression: *Expr,
        pub fn create(allocator: std.mem.Allocator, expr: *Expr) !*Stmt {
            const expression = try allocator.create(Expression);
            expression.* = .{ .expression = expr };
            return Stmt.create(allocator, .{ .expression = expression });
        }
    };
    pub const Print = struct {
        expression: *Expr,
        pub fn create(allocator: std.mem.Allocator, expression: *Expr) !*Stmt {
            const print = try allocator.create(Print);
            print.* = .{ .expression = expression };
            return Stmt.create(allocator, .{ .print = print });
        }
    };
    pub const Var = struct {
        name: Token,
        initializer: *Expr,
        pub fn create(allocator: std.mem.Allocator, name: Token, initializer: *Expr) !*Stmt {
            const variable = try allocator.create(Var);
            variable.* = .{ .name = name, .initializer = initializer };
            return Stmt.create(allocator, .{ .variable = variable });
        }
    };
    pub const Block = struct {
        statements: []*Stmt,
        pub fn create(allocator: std.mem.Allocator, statements: []*Stmt) !*Stmt {
            const block = try allocator.create(Block);
            block.* = .{ .statements = statements };
            return Stmt.create(allocator, .{ .block = block });
        }
        pub fn createWithArr(allocator: std.mem.Allocator, statementsArr: []*Stmt) !*Stmt {
            const block = try allocator.create(Block);
            const statements = try allocator.dupe(*Stmt, statementsArr);
            block.* = .{ .statements = statements };
            return Stmt.create(allocator, .{ .block = block });
        }
    };
    pub const If = struct {
        condition: *Expr,
        // 0 = then, 1 = else
        then_branch: *Stmt,
        else_branch: ?*Stmt,
        pub fn create(allocator: std.mem.Allocator, condition: *Expr, then_branch: *Stmt, else_branch: ?*Stmt) !*Stmt {
            const if_stmt = try allocator.create(If);
            if_stmt.* = .{ .condition = condition, .then_branch = then_branch, .else_branch = else_branch };
            return Stmt.create(allocator, .{ .if_stmt = if_stmt });
        }
    };
    pub const While = struct {
        condition: *Expr,
        body: *Stmt,
        pub fn create(allocator: std.mem.Allocator, condition: *Expr, body: *Stmt) !*Stmt {
            const while_stmt = try allocator.create(While);
            while_stmt.* = .{ .condition = condition, .body = body };
            return Stmt.create(allocator, .{ .while_stmt = while_stmt });
        }
    };
    pub const Function = struct {
        name: Token,
        params: []Token,
        body: []*Stmt,
        pub fn create(allocator: std.mem.Allocator, name: Token, params: []Token, body: []*Stmt) !*Stmt {
            const function = try allocator.create(Function);
            function.* = .{ .name = name, .params = params, .body = body };
            return Stmt.create(allocator, .{ .function = function });
        }
    };
};
