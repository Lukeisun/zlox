const std = @import("std");
pub const RuntimeError = error{
    ExpectingNumbers,
    ExpectingNumber,
    NonMatchingTypes,
    ExpectingNumbersOrStrings,
    ExpectingBooleans,
    ExpectingStrings,
    DivisionByZero,
    UndefinedVariable,
};

pub const Error = struct {
    line: u32 = 0,
    where: []u8 = "",
    message: []const u8 = "",
    pub fn report(self: Error) void {
        std.log.err("[line: {d}] {s}: {s}\n", .{ self.line, self.where, self.message });
    }
};
