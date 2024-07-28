const std = @import("std");
const token = @import("token.zig");
const Expr = @import("expression.zig").Expr;
pub const Stmt = union(enum) {
    expression: *Expression,
    print: *Print,
    // pub fn checkVisitorAndReturnType(comptime V: anytype) type {
    //     if (@typeInfo(V) != .Struct) {
    //         @compileError("Expecting struct");
    //     }
    //     if (!@hasDecl(V, "ReturnType")) {
    //         @compileError("Visitor must have a ReturnType field");
    //     }
    //
    //     if (@TypeOf(V.ReturnType) != type) {
    //         @compileError("Visitor.ReturnType must be a type");
    //     }
    //     const required_methods = [_][]const u8{
    //         "visitExpressionStmt",
    //         "visitPrintStmt",
    //     };
    //
    //     inline for (required_methods) |method| {
    //         if (!@hasDecl(V, method)) {
    //             @compileError("Visitor is missing " ++ method ++ " method");
    //         }
    //         const return_type = @typeInfo(@TypeOf(@field(V, method))).Fn.return_type.?;
    //         if (return_type != V.ReturnType) {
    //             @compileError(method ++ " method must return Visitor.ReturnType");
    //         }
    //     }
    //     return V.ReturnType;
    // }
    pub fn create(allocator: std.mem.Allocator, stmt_data: anytype) !*Stmt {
        const stmt = try allocator.create(Stmt);
        stmt.* = stmt_data;
        return stmt;
    }
    pub fn accept(self: *Stmt, visitor: anytype) !void {
        return switch (self.*) {
            .expression => |e| try visitor.visitExpressionStmt(e),
            .print => |p| try visitor.visitPrintStmt(p),
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
};
