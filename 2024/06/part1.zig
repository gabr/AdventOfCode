const std = @import("std");
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

fn solve(reader: anytype) !u64 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    var line_it = std.mem.splitAny(u8, input_al.items, "\n");
    var lines_al = try std.ArrayList([]u8).initCapacity(allocator, 1014);
    while(line_it.next()) |line| {
        if (line.len == 0) continue;
        try lines_al.append(@constCast(line));
    }
    const map = lines_al.items;
    // find starting position - direction is always up
    const Direction = enum { up, right, down, left, };
    const dirs_count = @typeInfo(Direction).Enum.fields.len;
    var dir = Direction.up;
    var ri: isize = undefined;
    var ci: isize = undefined;
    start_pos: for (map,0..) |row,i| {
        for (row,0..) |c,j| {
            if (c == '^') {
                ri = @intCast(i);
                ci = @intCast(j);
                break :start_pos;
            }
        }
    }
    // move around and mark visited places
    const visited = 'X';
    map[@intCast(ri)][@intCast(ci)] = visited;
    while (true) {
        const pri = ri;
        const pci = ci;
        switch (dir) {
            .up    => ri -= 1,
            .right => ci += 1,
            .down  => ri += 1,
            .left  => ci -= 1,
        }
        if (ri < 0 or ri >= map.len or
            ci < 0 or ci >= map[0].len) break; // out of map
        // obstackle - go back, turn and try again
        if (map[@intCast(ri)][@intCast(ci)] == '#') {
            ri = pri;
            ci = pci;
            dir = @enumFromInt((@as(u8, @intFromEnum(dir))+1)%dirs_count);
            continue;
        }
        map[@intCast(ri)][@intCast(ci)] = visited;
    }
    var visited_count: usize = 0;
    for (map) |row| {
        for (row) |c| {
            if (c == visited) visited_count += 1;
        }
    }
    return visited_count;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example" { try test_solve(41,   "./06/example1.txt"); }
test "input"   { try test_solve(5101, "./06/input.txt"); }

