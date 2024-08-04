const std = @import("std");
const Token = @import("token.zig").Token;
const Literal = @import("token.zig").Literal;

const RuntimeError = @import("error.zig").RuntimeError;
pub const Environment = struct {
    enclosing: ?*Environment = null,
    map: std.StringHashMap(Literal),
    pub fn create(allocator: std.mem.Allocator) *Environment {
        const env = allocator.create(Environment) catch {
            std.debug.panic("OOM", .{});
        };
        const hm = std.StringHashMap(Literal).init(allocator);
        env.* = .{ .map = hm };
        return env;
    }
    pub fn createWithEnv(allocator: std.mem.Allocator, enclosing_env: *Environment) *Environment {
        const env = allocator.create(Environment) catch {
            std.debug.panic("OOM", .{});
        };
        const hm = std.StringHashMap(Literal).init(allocator);
        env.* = .{ .map = hm, .enclosing = enclosing_env };
        return env;
    }
    pub fn get(self: *Environment, name: Token) !Literal {
        const maybeLiteral = self.map.get(name.lexeme);
        if (maybeLiteral) |literal| {
            return literal;
        }
        if (self.enclosing) |enclosing| {
            return enclosing.get(name);
        }

        return RuntimeError.UndefinedVariable;
    }
    pub fn define(self: *Environment, name: []const u8, value: Literal) void {
        self.map.put(name, value) catch {
            std.debug.panic("OOM", .{});
        };
    }
    pub fn assign(self: *Environment, name: Token, value: Literal) !void {
        if (self.map.contains(name.lexeme)) {
            self.map.put(name.lexeme, value) catch {
                std.debug.panic("OOM", .{});
            };
            return;
        }
        if (self.enclosing) |enclosing| {
            return enclosing.assign(name, value);
        }
        return RuntimeError.UndefinedVariable;
    }
    pub fn print(self: *Environment) void {
        var it = self.map.iterator();
        while (it.next()) |i| {
            var buf: [128]u8 = undefined;
            std.debug.print("KEY: {s} VAL: {s}\n", .{ i.key_ptr.*, i.value_ptr.*.toString(&buf) catch unreachable });
        }
    }
};
