const std = @import("std");
const Token = @import("token.zig").Token;
const Literal = @import("token.zig").Literal;

const RuntimeError = @import("error.zig").RuntimeError;
pub const Environment = struct {
    enclosing: ?*Environment = null,
    map: std.StringHashMap(Literal),
    pub fn create(allocator: std.mem.Allocator) Environment {
        const hm = std.StringHashMap(Literal).init(allocator);
        return .{ .map = hm };
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
};
