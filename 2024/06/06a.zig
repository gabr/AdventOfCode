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
    var lines_al = try std.ArrayList([]const u8).initCapacity(allocator, 1014);
    while(line_it.next()) |line| {
        if (line.len == 0) continue;
        try lines_al.append(line);
    }
    const rows = lines_al.items;
    // find starting position - direction is always up
    const Direction = enum { up, down, left, right, };
    var direction = Direction.up;
    var pos: [2]isize = undefined;
    for (rows,0..) |row,ri| {
        for (row,0..) |c,ci| {
            if (c == '^') {
                pos[0] = @intCast(ri);
                pos[1] = @intCast(ci);
                break;
            }
        }
    }
    var moves: usize = 1;
    var visited_pos = try allocator.alloc([]bool, rows.len);
    for (0..rows.len) |i| {
        visited_pos[i] = try allocator.alloc(bool, rows[0].len);
        for (0..rows[0].len) |j| {
            visited_pos[i][j] = false;
        }
    }
    visited_pos[@intCast(pos[0])][@intCast(pos[1])] = true;
    while (true) {
        const np = switch (direction) {
            .up    => .{pos[0]-1, pos[1]},
            .down  => .{pos[0]+1, pos[1]},
            .left  => .{pos[0],   pos[1]-1},
            .right => .{pos[0],   pos[1]+1},
        };
        if (np[0] == -1 or np[0] == rows.len or
            np[1] == -1 or np[1] == rows[0].len) break;
        if (rows[@intCast(np[0])][@intCast(np[1])]=='#') {
            direction = switch (direction) {
                .up    => .right,
                .down  => .left,
                .left  => .up,
                .right => .down,
            };
            continue;
        }
        if (!visited_pos[@intCast(np[0])][@intCast(np[1])]) {
            moves+=1;
            visited_pos[@intCast(np[0])][@intCast(np[1])] = true;
        }
        pos = np;
    }
    return moves;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "06a example.a.txt" { try test_solve(41,   "./06/example.a.txt"); }
test "06a input.txt"     { try test_solve(5101, "./06/input.txt"); }

