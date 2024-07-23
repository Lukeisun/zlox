const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const expr = @import("expression.zig");
pub fn runFile(allocator: std.mem.Allocator, filename: [:0]const u8) !void {
    const file = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    const stat = try file.stat();
    const buff = try file.readToEndAlloc(allocator, stat.size);
    const tokens = try lexer.lex(allocator, buff);
    defer tokens.deinit();
    for (tokens.items) |t| {
        try t.print();
    }
}
pub fn runPrompt(allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.writeAll("> ");
    while (try stdin.readUntilDelimiterOrEofAlloc(allocator, '\n', 128)) |s| {
        defer allocator.free(s);
        try stdout.print("{s}\n", .{s});
        try stdout.writeAll("> ");
    }
}
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    var collected_args = std.ArrayList([:0]const u8).init(allocator);
    _ = args.skip();
    while (args.next()) |arg| {
        try collected_args.append(arg);
    }
    switch (collected_args.items.len) {
        1 => {
            try runFile(allocator, collected_args.items[0]);
        },
        0 => {
            try runPrompt(allocator);
        },
        else => {
            const stderr = std.io.getStdErr().writer();
            try stderr.writeAll("Usage: zlox [script]\n");
        },
    }
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();
    const expression = try expr.Expr.Binary.create(
        arena_allocator,
        try expr.Expr.Unary.create(
            arena_allocator,
            try expr.Expr.Literal.create(allocator, token.Literal{ .number = 123 }),
            token.Token{ .type = token.TokenType.MINUS, .lexeme = "-", .line = 1, .literal = token.Literal.null },
        ),
        token.Token{ .type = token.TokenType.STAR, .lexeme = "*", .line = 1, .literal = token.Literal.null },
        try expr.Expr.Grouping.create(
            arena_allocator,
            try expr.Expr.Literal.create(
                arena_allocator,
                token.Literal{ .number = 45.67 },
            ),
        ),
    );
    // const left = try expr.Expr.Literal.create(allocator, token.Literal{ .number = 1 });
    // const right = try expr.Expr.Literal.create(allocator, token.Literal{ .number = 1 });
    // const ex = try expr.Expr.Binary.create(
    //     allocator,
    //     left,
    //     token.Token{ .type = token.TokenType.MINUS, .lexeme = "-", .line = 1, .literal = token.Literal.null },
    //     right,
    // );
    // defer ex.destruct(allocator);
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();
    const visit = expr.PrintVisitor{ .output = &output };
    visit.print(expression);
    std.debug.print("{s}\n", .{output.items});
    // t.print(ex);
}
