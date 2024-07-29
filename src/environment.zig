const std = @import("std");
const Token = @import("token.zig").Token;
const Literal = @import("token.zig").Literal;

const RuntimeError = @import("error.zig").RuntimeError;
pub const Environment = struct {
    map: *std.StringHashMapUnmanaged(Literal),
    pub fn create(allocator: std.mem.Allocator) *Environment {
        const hm_ptr = allocator.create(std.StringHashMapUnmanaged(Literal)) catch {
            std.debug.panic("OOM", .{});
        };
        hm_ptr.* = .{};
        const env = allocator.create(Environment) catch {
            std.debug.panic("OOM", .{});
        };
        env.* = .{ .map = hm_ptr };
        return env;
    }
    pub fn get(self: *Environment, name: Token) !Literal {
        return self.map.get(name.lexeme) orelse return RuntimeError.UndefinedVariable;
    }
    pub fn define(self: *Environment, allocator: std.mem.Allocator, name: []const u8, value: Literal) void {
        self.map.put(allocator, name, value) catch {
            std.debug.panic("OOM", .{});
        };
    }
};
