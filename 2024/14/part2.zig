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

    pub fn step(self: *Drone, width: usize, height: usize) void {
        self.stepBy(width, height, 1);
    }

    pub fn stepBy(self: *Drone, width: usize, height: usize, steps: usize) void {
        self.p0 = moveBy(width,  steps, self.p0, self.v0);
        self.p1 = moveBy(height, steps, self.p1, self.v1);
    }

    fn moveBy(max: usize, steps: usize, pos: usize, vel: i128) usize {
        const s: i128 = @intCast(steps);
        const p: i128 = @intCast(pos);
        const m: i128 = @intCast(max);
        const np = @rem(@rem(p+s*vel, m) + m, m);
        return @intCast(np);
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
        try drones_al.append(.{
            .p0 = try std.fmt.parseInt(usize, pos_it.next().?, 10),
            .p1 = try std.fmt.parseInt(usize, pos_it.next().?, 10),
            .v0 = try std.fmt.parseInt(i128,  vel_it.next().?, 10),
            .v1 = try std.fmt.parseInt(i128,  vel_it.next().?, 10),
        });
    }
    const drones = drones_al.items;
    for (drones) |*d| {
        d.stepBy(width, height, 8050);
    }
    var map = try allocator.alloc([]u8, height);
    for (0..height) |h| {
        map[h] = try allocator.alloc(u8, width);
        for (map[h]) |*pos| { pos.*='.'; }
    }
    for (drones)    |d| { map[d.p1][d.p0] = '#'; }
    for (0..height) |h| { dprint("{s}\n", .{map[h]}); }
    return 0;
}

fn test_solve(expected: u128, width: usize, height: usize, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(width, height, file.reader()));
}
//test "example1" { try test_solve(12, 11, 7, "./14/example1.txt"); }
//test "input"    { try test_solve(215987200, 101, 103, "./14/input.txt"); }
