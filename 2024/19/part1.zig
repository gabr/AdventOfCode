const std = @import("std");
const math = std.math;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const isWhitespace = std.ascii.isWhitespace;
//const dprint = std.debug.print;
fn dprint(comptime fmt: []const u8, args: anytype) void { _=fmt; _=args; }

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

var stack: [255][2]usize = undefined;
var failed: [255][2]usize = undefined;
fn hasFailed(l: usize, r: usize, fi: usize) bool {
    for (failed[0..fi]) |fail| {
        if (fail[0] == l and fail[1] == r) {
            return true;
        }
    }
    return false;
}

fn isPossible(pattern: []const u8, colors_map: std.StringHashMap(void), max: usize) !bool {
    var si: usize = 0;
    var fi: usize = 0;
    var l: usize = 0;
    var r: usize = pattern.len;
    if (r > max) { r = max; }
    var sl = l;
    var sr = r;
    dprint("testing long: '{s}'", .{pattern});
    while (true) {
        const color = pattern[l..r];
        if (color.len > max) { r -= 1; continue; }
        dprint("  testing: [{d},{d}] '{s}'", .{l, r, color});
        if (colors_map.contains(color)) {
            dprint(" - found\n", .{});
            if (color.len > 1 and r > 0) {
                stack[si] = .{ l, r-1 }; si += 1;
            }
            l = r;
            r = pattern.len;
            if (r-l > max) { r = l+max; }
            if (l >= pattern.len) return true;
        } else {
            dprint(" - not found\n", .{});
            r -= 1;
            if (r <= l) {
                failed[fi] = .{sl, sr}; fi += 1;
                while (true) {
                    if (si == 0) return false;
                    dprint("   stack:  {any}\n", .{stack[0..si]});
                    dprint("   failed: {any}\n", .{failed[0..fi]});
                    si -= 1;
                    l = stack[si][0];
                    r = stack[si][1];
                    dprint("   back tracking to: [{d}, {d}]\n", .{l, r});
                    if (hasFailed(l, r, fi)) {
                        dprint("   failed\n", .{});
                    } else {
                        sl = l;
                        sr = r;
                        break;
                    }
                }
            }
        }
    }
    unreachable;
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
    //var colors_it = colors_map.iterator();
    //dprint("colors ({d}):\n", .{colors_map.count()});
    //while (colors_it.next()) |entry| {
    //    dprint(" color: {s}\n", .{entry.key_ptr.*});
    //}
    var count: usize = 0;
    while(lines_it.next()) |line_to_trim| {
        const line = std.mem.trim(u8, line_to_trim, "\r \t");
        if (line.len == 0) continue;
        if (try isPossible(line, colors_map, longest_color)) {
            dprint("possible: {s}\n", .{line});
            count += 1;
        } else {
            dprint("not possible: {s}\n", .{line});
        }
    }
    return count;
}

fn test_solve(expected: usize, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example1" { try test_solve(6,   "./19/example1.txt"); }
test "input"    { try test_solve(311, "./19/input.txt"); }
// too low: 283, 285
