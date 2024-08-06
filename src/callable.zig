const std = @import("std");

const Literal = @import("token.zig").Literal;
const Expr = @import("expression.zig").Expr;
const Interpreter = @import("interpreter.zig").Interpreter;
const Environment = @import("environment.zig").Environment;
const Stmt = @import("statement.zig").Stmt;
const RuntimeError = @import("error.zig").RuntimeError;
const Instance = @import("class.zig").Instance;

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
    pub fn toString(self: Callable) []const u8 {
        switch (self) {
            .function => |f| return f.toString(),
            .clock => |c| return c.toString(),
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
    closure: *Environment,
    allocator: std.mem.Allocator,
    initializer: bool = false,
    fn call(self: LoxFunction, interpreter: *Interpreter, arguments: []Literal) Interpreter.ExprReturnType {
        var env = Environment.createWithEnv(self.allocator, self.closure);
        for (self.declaration.params, 0..) |param, i| {
            env.define(param.lexeme, arguments[i]);
        }
        interpreter.executeBlock(self.declaration.body, env) catch |err| {
            if (err == RuntimeError.Return) {
                if (self.initializer) return self.closure.getAt(0, "this");
                const value = interpreter.return_value.?;
                interpreter.return_value = null;
                return value;
            }
            return err;
        };
        if (self.initializer) return self.closure.getAt(0, "this");
        return Literal{ .null = {} };
    }
    pub fn arity(self: LoxFunction) usize {
        return self.declaration.params.len;
    }
    pub fn bind(self: LoxFunction, instance: *Instance) Literal {
        const env = Environment.create(self.allocator);
        env.define("this", Literal{ .instance = instance });
        const lf = LoxFunction.create(self.allocator, self.declaration, env, self.initializer) catch {
            std.debug.panic("OOM", .{});
        };
        const _callable = lf.callable() catch {
            std.debug.panic("OOM", .{});
        };
        return Literal{ .callable = _callable };
    }
    pub fn create(allocator: std.mem.Allocator, declaration: *Stmt.Function, closure: *Environment, initializer: bool) !*LoxFunction {
        const fun = try allocator.create(LoxFunction);
        fun.* = .{ .allocator = allocator, .declaration = declaration, .closure = closure, .initializer = initializer };
        return fun;
    }
    pub fn toString(self: *@This()) []const u8 {
        return std.fmt.allocPrint(self.allocator, "<fn {s}>", .{self.declaration.name.lexeme}) catch {
            std.debug.panic("OOM", .{});
        };
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
    pub fn toString(_: @This()) []const u8 {
        const s: []const u8 = "<native fn>";
        return s;
    }
};
