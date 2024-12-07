const std = @import("std");
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

fn xmasInDir(rows: [][]const u8, ri: usize, ci: usize, dr: isize, dc: isize) bool {
    const xmas = "XMAS";
    var chars: [xmas.len]u8 = undefined;
    var i: usize = 0;
    var rid: isize = @intCast(ri);
    var cid: isize = @intCast(ci);
    while (true) {
        chars[i] = rows[@intCast(rid)][@intCast(cid)];
        i += 1;
        if (i == xmas.len) break;
        // advance and check for out of bounds
        rid += dr;
        cid += dc;
        if (rid < 0 or rid >= rows.len or
            cid < 0 or cid >= rows[0].len) return false;
    }
    return std.mem.eql(u8, xmas, &chars);
}

fn solve(reader: anytype) !u64 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    var line_it = std.mem.splitAny(u8, input_al.items, "\n\r");
    var rows_al = try std.ArrayList([]const u8).initCapacity(allocator, 1014);
    while(line_it.next()) |line| {
        if (line.len == 0) continue;
        try rows_al.append(line);
    }
    const rows = rows_al.items;
    var count: u64 = 0;
    for (0..rows.len) |ri| {
        for (0..rows[0].len) |ci| {
            if (xmasInDir(rows, ri, ci,  0, -1)) count += 1;
            if (xmasInDir(rows, ri, ci,  0,  1)) count += 1;
            if (xmasInDir(rows, ri, ci,  1,  0)) count += 1;
            if (xmasInDir(rows, ri, ci, -1,  0)) count += 1;
            if (xmasInDir(rows, ri, ci, -1, -1)) count += 1;
            if (xmasInDir(rows, ri, ci, -1,  1)) count += 1;
            if (xmasInDir(rows, ri, ci,  1, -1)) count += 1;
            if (xmasInDir(rows, ri, ci,  1,  1)) count += 1;
        }
    }
    return count;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example" { try test_solve(18,   "./04/example1.txt"); }
test "input"   { try test_solve(2532, "./04/input.txt"); }

