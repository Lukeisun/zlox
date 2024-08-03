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
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const file = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    const stat = try file.stat();
    defer file.close();
    const arena_allocator = arena.allocator();
    const source = try file.readToEndAlloc(arena_allocator, stat.size);
    std.debug.assert(source.len != 0);
    const tokens = try lexer.lex(arena_allocator, source);
    // try token.debugTokens(tokens);
    const statements = Parser.parse(arena_allocator, tokens);
    var interpreter = EvalVisitor.create(arena_allocator);
    interpreter.interpret(statements) catch {};
}
pub fn runPrompt(allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();
    var interpreter = EvalVisitor.create(arena_allocator);
    interpreter.repl = true;
    try stdout.writeAll("> ");
    // could just replace this with streamuntildelimitter line in the stdlib
    while (try stdin.readUntilDelimiterOrEofAlloc(arena_allocator, '\n', 1024)) |s| {
        const tokens = try lexer.lex(arena_allocator, s);
        // try token.debugTokens(tokens.items);
        const statements = Parser.parse(arena_allocator, tokens);
        if (statements.len == 0) {
            try stdout.writeAll("> ");
            continue;
        }
        interpreter.interpret(statements) catch {
            try stdout.writeAll("> ");
            continue;
        };
        try stdout.writeAll("> ");
    }
    // _ = arena.reset(.retain_capacity);
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
test "Run" {
    const allocator = std.testing.allocator;
    // var buf: [1024]u8 = undefined;
    _ = std.io.getStdOut();
    // _ = try stdout.readAll(&buf);
    try runFile(allocator, "testfiles/for.lox");
    // std.debug.print("{s}", .{buf});
}
