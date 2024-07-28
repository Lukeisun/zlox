const std = @import("std");
pub const Error = struct {
    line: u32 = 0,
    where: []u8 = "",
    message: [:0]const u8 = "",
    pub fn report(self: Error) void {
        std.log.err("[line: {d}] {s}: {s}\n", .{ self.line, self.where, self.message });
    }
};
