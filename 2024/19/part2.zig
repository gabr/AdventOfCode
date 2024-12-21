const std = @import("std");
const math = std.math;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const isWhitespace = std.ascii.isWhitespace;
const dprint = std.debug.print;
//fn dprint(comptime fmt: []const u8, args: anytype) void { _=fmt; _=args; }

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

// singleton global general purpose allocator
fn gpa() Allocator {
    const GPA = std.heap.GeneralPurposeAllocator(.{});
    const S = struct { var gpa: ?GPA = null; };
    if (S.gpa == null) { S.gpa = GPA{}; }
    return S.gpa.?.allocator();
}

var cache = [_]u64 {0} ** 255;
fn countPossible(pattern: []const u8, n: usize, colors_map: std.StringHashMap(void), max: usize) !usize {
    // reset cache when starting
    if (n == 0) { for (0..cache.len) |i| { cache[i] = 0; } }
    if (pattern[n..].len == 0) return 1; // reached the end
    if (cache[n] > 0) return cache[n] - 1; // already have been here
    cache[n] = 1; // mark visited
    for (0..max+1) |i| {
        if (n+i >= pattern.len) break;
        if (colors_map.contains(pattern[n..n+i+1])) {
            cache[n] += try countPossible(pattern, n+i+1, colors_map, max);
        }
    }
    return cache[n] - 1;
}

fn solve(reader: anytype) !usize {
    const input = try reader.readAllAlloc(gpa(), std.math.maxInt(usize));
    var lines_it = mem.splitScalar(u8, input, '\n');
    var colors_map = std.StringHashMap(void).init(gpa());
    var longest_color: usize = 0;
    while(lines_it.next()) |line_to_trim| {
        const line = std.mem.trim(u8, line_to_trim, "\r \t");
        if (line.len == 0) break;
        var colors_it = std.mem.splitSequence(u8, line, ", ");
        while (colors_it.next()) |color| {
            if (color.len > longest_color) { longest_color = color.len; }
            try colors_map.put(color, {});
        }
    }
    var count: usize = 0;
    while(lines_it.next()) |line_to_trim| {
        const line = std.mem.trim(u8, line_to_trim, "\r \t");
        if (line.len == 0) continue;
        const cp = try countPossible(line, 0, colors_map, longest_color);
        //dprint("possible count for '{s}': {d}\n", .{line, cp});
        count += cp;
    }
    return count;
}

fn test_solve(expected: usize, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example1" { try test_solve(16,   "./19/example1.txt"); }
test "input"    { try test_solve(616234236468263, "./19/input.txt"); }
// too low: 1567249600
//         16583199666
//     616234236468263
