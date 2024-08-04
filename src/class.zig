const std = @import("std");

pub const Class = struct {
    name: []const u8,
    pub fn create(name: []const u8) Class {
        return .{ .name = name };
    }
    pub fn toString(self: Class) []const u8 {
        return self.name;
    }
};
