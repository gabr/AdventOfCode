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
        const p0 = try std.fmt.parseInt(i128, pos_it.next().?, 10);
        const p1 = try std.fmt.parseInt(i128, pos_it.next().?, 10);
        const v0 = try std.fmt.parseInt(i128, vel_it.next().?, 10);
        const v1 = try std.fmt.parseInt(i128, vel_it.next().?, 10);
        const s0 = p0+(seconds*v0);
        const s1 = p1+(seconds*v1);
        const m0: usize = @intCast(if (s0 < 0) -s0 else s0);
        const m1: usize = @intCast(if (s1 < 0) -s1 else s1);
        var dp0 = m0 % width;
        var dp1 = m1 % height;
        if (s0 < 0 and dp0 != 0) { dp0 = width  - dp0; }
        if (s1 < 0 and dp1 != 0) { dp1 = height - dp1; }
        const d0: usize = @intCast(dp0);
        const d1: usize = @intCast(dp1);
        var quadranti_op: ?usize = null;
             if (d0 < (width-1)/2 and d1 < (height-1)/2) { quadranti_op = 0; }
        else if (d0 > (width-1)/2 and d1 < (height-1)/2) { quadranti_op = 1; }
        else if (d0 < (width-1)/2 and d1 > (height-1)/2) { quadranti_op = 2; }
        else if (d0 > (width-1)/2 and d1 > (height-1)/2) { quadranti_op = 3; }
        dprint("p={d},{d} v={d},{d} -> {d}, {d} " ++
            "(s: {d}, {d}, m: {d}, {d}, dp: {d}, {d}) [{any}]\n", .{
            p0, p1,
            v0, v1,
            d0, d1,
            s0, s1,
            m0, m1,
            dp0, dp1,
            quadranti_op});
        if (quadranti_op) |qi| {
            quadrants[qi]+= 1;
        }
    }
    var total: u128 = 1;
    dprint("quadrants: {any}\n:=", .{quadrants});
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
