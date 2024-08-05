const std = @import("std");
const Literal = @import("token.zig").Literal;
const Token = @import("token.zig").Token;
const Interpreter = @import("interpreter.zig").Interpreter;
const ExprReturnType = @import("interpreter.zig").Interpreter.ExprReturnType;
const RuntimeError = @import("error.zig").RuntimeError;

pub const Class = struct {
    name: []const u8,
    allocator: std.mem.Allocator,
    pub fn create(allocator: std.mem.Allocator, name: []const u8) !*Class {
        const klass = try allocator.create(Class);
        klass.* = .{ .name = name, .allocator = allocator };
        return klass;
    }
    pub fn toString(self: Class) []const u8 {
        return self.name;
    }
    pub fn call(self: Class, _: *Interpreter, _: []Literal) ExprReturnType {
        const instance = try Instance.create(self.allocator, self);
        return Literal{ .instance = instance };
    }
    pub fn arity(_: Class) usize {
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
    pub fn get(self: Instance, name: Token) !Literal {
        if (self.fields.contains(name.lexeme)) {
            return self.fields.get(name.lexeme).?;
        }
        return RuntimeError.UndefinedProperty;
    }
};
