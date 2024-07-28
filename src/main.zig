const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const expr = @import("expression.zig");
const Parser = @import("parser.zig").Parser;
const EvalVisitor = @import("interpreter.zig").EvalVisitor;
pub const keywords = std.StaticStringMap(token.TokenType).initComptime(.{
    .{ "and", token.TokenType.AND },
    .{ "class", token.TokenType.CLASS },
    .{ "else", token.TokenType.ELSE },
    .{ "false", token.TokenType.FALSE },
    .{ "for", token.TokenType.FOR },
    .{ "fun", token.TokenType.FUN },
    .{ "if", token.TokenType.IF },
    .{ "nil", token.TokenType.NIL },
    .{ "or", token.TokenType.OR },
    .{ "print", token.TokenType.PRINT },
    .{ "return", token.TokenType.RETURN },
    .{ "super", token.TokenType.SUPER },
    .{ "this", token.TokenType.THIS },
    .{ "true", token.TokenType.TRUE },
    .{ "var", token.TokenType.VAR },
    .{ "while", token.TokenType.WHILE },
});
pub fn runFile(allocator: std.mem.Allocator, filename: [:0]const u8) !void {
    const file = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    const stat = try file.stat();
    const source = try file.readToEndAlloc(allocator, stat.size);
    const tokens = try lexer.lex(allocator, source);
    defer tokens.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();
    const statements = Parser.parse(arena_allocator, tokens.items);
    var output = std.ArrayList(u8).init(arena_allocator);
    defer output.deinit();
    var interpreter = EvalVisitor.create(arena_allocator);
    try interpreter.interpret(statements);
}
pub fn runPrompt(allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();
    try stdout.writeAll("> ");
    while (try stdin.readUntilDelimiterOrEofAlloc(allocator, '\n', 128)) |s| {
        defer allocator.free(s);
        const tokens = try lexer.lex(allocator, s);
        defer tokens.deinit();
        // try token.debugTokens(tokens.items);
        const statements = Parser.parse(arena_allocator, tokens.items);
        if (statements.items.len == 0) {
            try stdout.writeAll("> ");
            continue;
        }
        var interpreter = EvalVisitor.create(arena_allocator);
        try interpreter.interpret(statements);
        try stdout.writeAll("> ");
        _ = arena.reset(.retain_capacity);
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
}
