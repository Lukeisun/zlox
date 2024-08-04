const std = @import("std");

const Literal = @import("token.zig").Literal;
const Expr = @import("expression.zig").Expr;
const Interpreter = @import("interpreter.zig").Interpreter;
const Environment = @import("environment.zig").Environment;
const Stmt = @import("statement.zig").Stmt;
const RuntimeError = @import("error.zig").RuntimeError;

pub const Callable = union(enum) {
    function: *LoxFunction,
    clock: Clock,
    pub fn call(self: Callable, interpreter: *Interpreter, arguments: []Literal) Interpreter.ExprReturnType {
        switch (self) {
            .function => |f| return f.call(interpreter, arguments),
            .clock => |c| return c.call(interpreter, arguments),
        }
    }
    pub fn arity(self: Callable) usize {
        switch (self) {
            .function => |f| return f.arity(),
            .clock => |c| return c.arity(),
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
    fn call(self: LoxFunction, interpreter: *Interpreter, arguments: []Literal) Interpreter.ExprReturnType {
        var env = Environment.createWithEnv(self.allocator, interpreter.globals);
        for (self.declaration.params, 0..) |param, i| {
            env.define(param.lexeme, arguments[i]);
        }
        interpreter.executeBlock(self.declaration.body, env) catch |err| {
            if (err == RuntimeError.Return) {
                const value = interpreter.return_value.?;
                interpreter.return_value = null;
                return value;
            }
            return err;
        };
        return Literal{ .null = {} };
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
pub const Clock = struct {
    fn arity(_: Clock) usize {
        return 0;
    }
    pub fn call(_: Clock, _: anytype, _: anytype) Interpreter.ExprReturnType {
        const time: f64 = @floatFromInt(std.time.timestamp());
        return Literal{
            .number = time,
        };
    }
};
