const std = @import("std");
pub fn runFile(filename: []u8) !void {
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;
    const file = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    const stat = try file.stat();
    const buff = try file.readToEndAlloc(allocator, stat.size);
    for (buff) |c| {
        try stdout.print("{c}", .{c});
    }
    try stdout.writeAll(buff);
}
pub fn runPrompt() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    const allocator = std.heap.page_allocator;
    // const z = stdin.read
    try stdout.writeAll("> ");
    while (try stdin.readUntilDelimiterOrEofAlloc(allocator, '\n', 128)) |s| {
        defer allocator.free(s);
        try stdout.print("{s}\n", .{s});
        try stdout.writeAll("> ");
    }
}
pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const args = try std.process.argsAlloc(allocator);
    switch (args.len) {
        2 => {
            try runFile(args[1]);
        },
        1 => {
            try runPrompt();
        },
        else => {
            const stderr = std.io.getStdErr().writer();
            try stderr.writeAll("Usage: zlox [script]\n");
        },
    }
    std.process.argsFree(allocator, args);
}
