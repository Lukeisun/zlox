const std = @import("std");
const Literal = @import("token.zig").Literal;
const Token = @import("token.zig").Token;
const Interpreter = @import("interpreter.zig").Interpreter;
const ExprReturnType = @import("interpreter.zig").Interpreter.ExprReturnType;
const Callable = @import("callable.zig").Callable;
const LoxFunction = @import("callable.zig").LoxFunction;
const RuntimeError = @import("error.zig").RuntimeError;
const Environment = @import("environment.zig").Environment;

pub const Class = struct {
    name: []const u8,
    allocator: std.mem.Allocator,
    methods: std.StringHashMap(*Callable),
    superclass: ?Literal,
    pub fn create(allocator: std.mem.Allocator, name: []const u8, methods: std.StringHashMap(*Callable), superclass: ?Literal) !*Class {
        const klass = try allocator.create(Class);
        klass.* = .{ .name = name, .allocator = allocator, .methods = methods, .superclass = superclass };
        return klass;
    }
    pub fn toString(self: Class) []const u8 {
        return self.name;
    }
    pub fn findMethod(self: Class, name: []const u8) ?*Callable {
        if (self.methods.get(name)) |ret| {
            return ret;
        }
        if (self.superclass) |superclass| {
            return superclass.class.findMethod(name);
        }
        return null;
    }
    pub fn call(self: Class, interpreter: *Interpreter, arguments: []Literal) ExprReturnType {
        const instance = try Instance.create(self.allocator, self);
        const initializer = self.findMethod("init");
        if (initializer) |i| {
            _ = try i.function.bind(instance).callable.call(interpreter, arguments);
        }
        return Literal{ .instance = instance };
    }
    pub fn arity(self: Class) usize {
        const initializer = self.findMethod("init");
        if (initializer) |i| {
            return i.function.arity();
        }
        return 0;
    }
};
pub const Instance = struct {
    klass: Class,
    fields: std.StringHashMap(Literal),
    allocator: std.mem.Allocator,
    pub fn create(allocator: std.mem.Allocator, klass: Class) !*Instance {
        const instance = try allocator.create(Instance);
        const fields = std.StringHashMap(Literal).init(allocator);
        instance.* = .{ .klass = klass, .allocator = allocator, .fields = fields };
        return instance;
    }
    pub fn toString(self: Instance) ![]const u8 {
        return std.fmt.allocPrint(self.allocator, "{s} instance", .{self.klass.name});
    }
    pub fn get(self: *Instance, name: Token) !Literal {
        if (self.fields.contains(name.lexeme)) {
            return self.fields.get(name.lexeme).?;
        }
        const maybeMethod = self.klass.findMethod(name.lexeme);
        if (maybeMethod) |m| return m.function.bind(self); //Literal{ .callable = m };
        return RuntimeError.UndefinedProperty;
    }
    pub fn set(self: *Instance, name: Token, value: Literal) void {
        self.fields.put(name.lexeme, value) catch {
            std.debug.panic("OOM", .{});
        };
    }
};
