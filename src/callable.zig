const std = @import("std");

const Literal = @import("token.zig").Literal;
const Expr = @import("expression.zig").Expr;
const EvalVisitor = @import("interpreter.zig").EvalVisitor;
const Environment = @import("environment.zig").Environment;
const Stmt = @import("statement.zig").Stmt;

pub const Callable = union(enum) {
    function: *LoxFunction,
    pub fn call(self: Callable, interpreter: *EvalVisitor, arguments: []Literal) EvalVisitor.ExprReturnType {
        switch (self) {
            .function => |f| return f.call(interpreter, arguments),
        }
    }
    pub fn arity(self: Callable) usize {
        switch (self) {
            .function => |f| return f.arity(),
        }
    }
    pub fn create(allocator: std.mem.Allocator, callable_data: anytype) !*Callable {
        const callable = try allocator.create(Callable);
        callable.* = callable_data;
        return callable;
    }
};

pub const LoxFunction = struct {
    declaration: *Stmt.Function,
    allocator: std.mem.Allocator,
    fn call(self: LoxFunction, interpreter: *EvalVisitor, arguments: []Literal) EvalVisitor.ExprReturnType {
        var env = Environment.create(self.allocator);
        for (self.declaration.params, 0..) |param, i| {
            env.define(param.lexeme, arguments[i]);
        }
        try interpreter.executeBlock(self.declaration.body, &env);
        return Literal{ .string = "test" };
    }
    fn arity(self: LoxFunction) usize {
        return self.declaration.params.len;
    }
    pub fn create(allocator: std.mem.Allocator, declaration: *Stmt.Function) !*LoxFunction {
        const fun = try allocator.create(LoxFunction);
        fun.* = .{ .allocator = allocator, .declaration = declaration };
        return fun;
    }
    pub fn callable(self: *@This()) !*Callable {
        return Callable.create(self.allocator, .{ .function = self });
    }
};
