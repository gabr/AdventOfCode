const std = @import("std");
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

const empty_field = '.';
const mark = '#';

fn markAntinodes(map: [][] u8, marks: [][] u8, antena: u8) void {
    for (0..map.len) |ri1| {
    for (0..map[0].len) |ci1| {
        const a1 = map[ri1][ci1];
        if (a1 != antena) continue;
        for (ci1+1..map[0].len) |ci2| {
            const ri2 = ri1;
            const a2 = map[ri2][ci2];
            if (a2 != antena) continue;
            markForAntenas(map, marks, ri1, ci1, ri2, ci2);
        }
        for (ri1+1..map.len) |ri2| {
        for (0..map[0].len) |ci2| {
            const a2 = map[ri2][ci2];
            if (a2 != antena) continue;
            markForAntenas(map, marks, ri1, ci1, ri2, ci2);
        } }
    } }
}

fn markForAntenas(map: [][] u8, marks: [][] u8, ri1: usize, ci1: usize, ri2: usize, ci2: usize) void {
    const dr: isize = @as(isize, @intCast(ri2))-@as(isize, @intCast(ri1));
    const dc: isize = @as(isize, @intCast(ci2))-@as(isize, @intCast(ci1));
    const ar1 = @as(isize, @intCast(ri1)) - dr;
    const ac1 = @as(isize, @intCast(ci1)) - dc;
    if (ar1 >= 0 and ar1 < map.len and ac1 >= 0 and ac1 < map[0].len)
        marks[@as(usize, @intCast(ar1))][@as(usize, @intCast(ac1))] = mark;
    const ar2 = @as(isize, @intCast(ri2)) + dr;
    const ac2 = @as(isize, @intCast(ci2)) + dc;
    if (ar2 >= 0 and ar2 < map.len and ac2 >= 0 and ac2 < map[0].len)
        marks[@as(usize, @intCast(ar2))][@as(usize, @intCast(ac2))] = mark;
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
    var marks = try allocator.alloc([]u8, map.len);
    for (0..map.len) |ri| {
        marks[ri] = try allocator.alloc(u8, map[ri].len);
        for (0..map[ri].len) |ci| {
            marks[ri][ci] = empty_field;
        }
    }
    for (map) |row| {
        for (row) |c| {
            if (c == empty_field) continue;
            markAntinodes(map, marks, c);
        }
    }
    var total_antinodes: u64 = 0;
    //dprint("marks: \n", .{});
    for (marks) |row| {
        for (row) |c| {
            //dprint("{c}", .{c});
            if (c == mark) {
                total_antinodes += 1;
            }
        }
        //dprint("\n", .{});
    }
    return total_antinodes;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example" { try test_solve(14,  "./08/example1.txt"); }
test "input"   { try test_solve(271, "./08/input.txt"); }
// incorrect: 182
