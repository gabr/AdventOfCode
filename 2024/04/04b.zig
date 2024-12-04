const std = @import("std");
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

fn xmas(rows: [][]const u8, x: usize, y: usize) bool {
    return
        rows[y][x]=='A' and
        ((rows[y-1][x-1]=='M' and rows[y+1][x+1]=='S') or (rows[y-1][x-1]=='S' and rows[y+1][x+1]=='M')) and
        ((rows[y-1][x+1]=='M' and rows[y+1][x-1]=='S') or (rows[y-1][x+1]=='S' and rows[y+1][x-1]=='M'));
}

fn solve(reader: anytype) !u64 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8 ).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    var line_it = std.mem.splitAny(u8, input_al.items, "\n\r");
    var rows_al = try std.ArrayList([]const u8).initCapacity(allocator, 1014);
    while(line_it.next()) |line| {
        if (line.len == 0) continue;
        try rows_al.append(line);
    }
    const rows = rows_al.items;
    var counter: usize = 0;
    for (1..rows.len-1) |ri| {
        for (1..rows[0].len-1) |ci| {
            if (xmas(rows, ci, ri)) counter += 1;
        }
    }
    return counter;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "04b example.b.txt" { try test_solve(9, "./04/example.a.txt"); }
test "04b input.txt"     { try test_solve(1941, "./04/input.txt"); }

