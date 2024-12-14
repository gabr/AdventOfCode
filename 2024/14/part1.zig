const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
//const dprint = std.debug.print;
fn dprint(comptime fmt: []const u8, args: anytype) void { _=fmt; _=args; }

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(101, 103, stdin)});
}

fn move(max: usize, steps: usize, pos: usize, vel: i128) usize {
    const s: i128 = @intCast(steps);
    const p: i128 = @intCast(pos);
    const m: i128 = @intCast(max);
    const np = @rem(@rem(p+s*vel, m) + m, m);
    return @intCast(np);
}

fn solve(width: usize, height: usize, reader: anytype) !u128 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    var line_it = std.mem.splitAny(u8, input_al.items, "\n");
    dprint("debug:\n", .{});
    var quadrants: [4]u128 = .{0,0,0,0};
    const seconds = 100;
    while(line_it.next()) |line| {
        if (line.len == 0) continue;
        var space_it = std.mem.splitScalar(u8, line, ' ');
        var pos_it = std.mem.splitScalar(u8, space_it.next().?["p=".len..], ',');
        var vel_it = std.mem.splitScalar(u8, space_it.next().?["v=".len..], ',');
        const p0 = try std.fmt.parseInt(usize, pos_it.next().?, 10);
        const p1 = try std.fmt.parseInt(usize, pos_it.next().?, 10);
        const v0 = try std.fmt.parseInt(i128, vel_it.next().?, 10);
        const v1 = try std.fmt.parseInt(i128, vel_it.next().?, 10);
        const d0 = move(width,  seconds, p0, v0);
        const d1 = move(height, seconds, p1, v1);
        if (d0 == (width-1)/2 or d1 == (height-1)/2) continue;
        const qi = d0/(((width+1)/2))+((d1/((height+1)/2))*2);
        quadrants[qi]+= 1;
    }
    var total: u128 = 1;
    dprint("quadrants: {any}\n", .{quadrants});
    for (quadrants) |q| { total *= q; }
    return total;
}

fn test_solve(expected: u128, width: usize, height: usize, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(width, height, file.reader()));
}
test "example1" { try test_solve(12, 11, 7, "./14/example1.txt"); }
test "input"    { try test_solve(215987200, 101, 103, "./14/input.txt"); }
