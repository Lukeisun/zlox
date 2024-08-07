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
    OutOfMemory,
    NotFnOrClass,
    MismatchedArity,
    NonInstancePropertyAccess,
    UndefinedProperty,
    SuperClassMustBeClass,
    // Used to signal a return value
    Return,
};

pub const Error = struct {
    line: u32 = 0,
    where: []u8 = "",
    message: []const u8 = "",
    pub fn report(self: Error) void {
        std.log.err("[line: {d}] {s}: {s}\n", .{ self.line, self.where, self.message });
    }
};
