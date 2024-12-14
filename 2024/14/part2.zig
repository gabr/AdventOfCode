const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const dprint = std.debug.print;
//fn dprint(comptime fmt: []const u8, args: anytype) void { _=fmt; _=args; }

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(101, 103, stdin)});
}

const Drone = struct {
    p0: usize,
    p1: usize,
    v0: i128,
    v1: i128,

    pub fn move(self: *Drone, width: usize, height: usize) void {
        const sp0: i128 = @intCast(self.p0);
        const sp1: i128 = @intCast(self.p1);
        const s0 = sp0+self.v0;
        const s1 = sp1+self.v1;
        const m0: usize = @intCast(if (s0 < 0) -s0 else s0);
        const m1: usize = @intCast(if (s1 < 0) -s1 else s1);
        var dp0 = m0 % width;
        var dp1 = m1 % height;
        if (s0 < 0 and dp0 != 0) { dp0 = width  - dp0; }
        if (s1 < 0 and dp1 != 0) { dp1 = height - dp1; }
        const d0: usize = @intCast(dp0);
        const d1: usize = @intCast(dp1);
        self.p0 = d0;
        self.p1 = d1;
    }
};

fn solve(width: usize, height: usize, reader: anytype) !u128 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    var line_it = std.mem.splitAny(u8, input_al.items, "\n");
    var drones_al = try std.ArrayList(Drone).initCapacity(allocator, 1024*16);
    dprint("debug:\n", .{});
    while(line_it.next()) |line| {
        if (line.len == 0) continue;
        var space_it = std.mem.splitScalar(u8, line, ' ');
        var pos_it = std.mem.splitScalar(u8, space_it.next().?["p=".len..], ',');
        var vel_it = std.mem.splitScalar(u8, space_it.next().?["v=".len..], ',');
        const p0 = try std.fmt.parseInt(usize, pos_it.next().?, 10);
        const p1 = try std.fmt.parseInt(usize, pos_it.next().?, 10);
        const v0 = try std.fmt.parseInt(i128, vel_it.next().?, 10);
        const v1 = try std.fmt.parseInt(i128, vel_it.next().?, 10);
        try drones_al.append(.{
            .p0 = p0,
            .p1 = p1,
            .v0 = v0,
            .v1 = v1,
        });
    }
    var map = try allocator.alloc([]u8, height);
    for (0..height) |h| {
        map[h] = try allocator.alloc(u8, width);
    }
    for (0..height) |h| {
        for (0..width) |w| {
            map[h][w] = '.';
        }
    }
    const drones = drones_al.items;
    for (0..10000) |s| {
        dprint("{d}:\n", .{s});
        for (drones)    |d| { map[d.p1][d.p0] = '@'; }
        for (0..height) |h| { dprint("{s}\n", .{map[h]}); }
        for (drones)    |d| { map[d.p1][d.p0] = '.'; }
        for (drones) |*d| {
            d.move(width, height);
        }
    }
    return 0;
}

fn test_solve(expected: u128, width: usize, height: usize, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(width, height, file.reader()));
}
//test "example1" { try test_solve(12, 11, 7, "./14/example1.txt"); }
//test "input"    { try test_solve(215987200, 101, 103, "./14/input.txt"); }
