const std = @import("std");
const Token = @import("token.zig").Token;
const Literal = @import("token.zig").Literal;

const RuntimeError = @import("error.zig").RuntimeError;
pub const Environment = struct {
    map: std.StringHashMap(Literal),
    pub fn create(allocator: std.mem.Allocator) Environment {
        const map = std.StringHashMap(Literal).init(allocator);
        return .{ .map = map };
    }
    pub fn get(self: *Environment, name: Token) !Literal {
        return self.map.get(name.lexeme) orelse return RuntimeError.UndefinedVariable;
    }
    pub fn define(self: *Environment, name: []const u8, value: Literal) void {
        self.map.put(name, value) catch {
            std.debug.panic("OOM", .{});
        };
    }
};
