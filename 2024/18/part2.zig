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
    const res = try solve(1024, stdin);
    try stdout.print("{d},{d}\n", .{res[0], res[1]});
}

// singleton global general purpose allocator
fn gpa() Allocator {
    const GPA = std.heap.GeneralPurposeAllocator(.{});
    const S = struct { var gpa: ?GPA = null; };
    if (S.gpa == null) { S.gpa = GPA{}; }
    return S.gpa.?.allocator();
}

fn allocMap(comptime T: type, rows: usize, cols: usize, fill: T) ![][]T {
    const map = try gpa().alloc([]T, rows);
    for (0..rows) |ri| {
        map[ri] = try gpa().alloc(T, cols);
        for (0..cols) |ci| {
            map[ri][ci] = fill;
        }
    }
    return map;
}

fn parseInt(comptime T: type, str: []const u8) !T {
    return try std.fmt.parseInt(T, str, 10);
}

const Square = struct {
    type: Type = .empty,
    steps: usize = no_steps,

    pub const Type = enum { empty, wall };
    pub const no_steps = math.maxInt(usize);

    pub fn reset(self: *Square) void {
        self.type = .empty;
        self.steps = no_steps;
    }
};

const Pos = struct {
    ri: usize,
    ci: usize,

    pub fn fromStr(str: []const u8) !Pos {
        var it = mem.tokenizeScalar(u8, str, ',');
        const ci = try parseInt(usize, it.next().?);
        const ri = try parseInt(usize, it.next().?);
        return .{
            .ri = ri,
            .ci = ci,
        };
    }
};

const dirs= [_][2]i8 {
    .{ -1,  0 },
    .{  0,  1 },
    .{  1,  0 },
    .{  0, -1 },
};

pub fn movePos(pos: Pos, dir: [2]i8, map: [][]Square) ?Pos {
    const ri = @as(isize, @intCast(pos.ri)) + dir[0];
    const ci = @as(isize, @intCast(pos.ci)) + dir[1];
    if (ri < 0 or ci < 0 or ri >= map.len or ci >= map[0].len) return null;
    return .{
        .ri = @intCast(ri),
        .ci = @intCast(ci),
    };
}

fn walkMap(map: [][]Square, start_pos: Pos) !void {
    var stack = try std.ArrayList(Pos).initCapacity(gpa(), 1024);
    var tmp = try std.ArrayList(Pos).initCapacity(gpa(), 1024);
    try stack.append(start_pos);
    map[start_pos.ri][start_pos.ci].steps = 0;
    while (stack.items.len > 0) {
        const swap = tmp; tmp = stack; stack = swap;
        while (tmp.popOrNull()) |pos| {
            const steps = map[pos.ri][pos.ci].steps;
            for (dirs) |dir| {
                if (movePos(pos, dir, map)) |newpos| {
                    const newsquare = map[newpos.ri][newpos.ci];
                    if (newsquare.type == .wall) continue;
                    if (newsquare.steps <= steps + 1) continue;
                    map[newpos.ri][newpos.ci].steps = steps + 1;
                    try stack.append(newpos);
                }
            }
            //dprint("tmp   ({d}): {any}\n", .{tmp.items.len, tmp.items});
            //dprint("stack ({d}): {any}\n", .{stack.items.len, stack.items});
            //debugPrintMapSteps(map);
        }
    }
}

fn debugPrintMap(map: [][]Square) void {
    dprint("map:\n", .{});
    for (map) |row| {
        for (row) |square| {
            if (square.type == .wall) { dprint("#", .{}); }
            else                      { dprint(".", .{}); }
        }
        dprint("\n", .{});
    }
}

fn debugPrintMapSteps(map: [][]Square) void {
    dprint("map steps:\n", .{});
    for (map) |row| {
        for (row) |square| {
                 if (square.type == .wall)            { dprint("    #",   .{});             }
            else if (square.steps == Square.no_steps) { dprint("{d: >5}", .{-1});           }
            else                                      { dprint("{d: >5}", .{square.steps}); }
        }
        dprint("\n", .{});
    }
}

fn resetMap(map: [][]Square) void {
    for (0..map.len) |ri| {
        for (0..map[0].len) |ci| {
            map[ri][ci].reset();
        }
    }

}

fn hasExit(bytes: []Pos, count: usize, map: [][]Square, grid_size: usize) !bool {
    resetMap(map);
    var bytes_count: usize = 0;
    for (bytes) |pos| {
        map[pos.ri][pos.ci].type = .wall;
        bytes_count += 1;
        if (bytes_count == count) break;
    }
    try walkMap(map, .{ .ri = 0, .ci = 0 });
    return map[grid_size-1][grid_size-1].steps != Square.no_steps;
}

fn solve(grid_size: usize, reader: anytype) ![2]usize {
    const map = try allocMap(Square, grid_size, grid_size, .{});
    const input = try reader.readAllAlloc(gpa(), std.math.maxInt(usize));
    var bytes_al = try std.ArrayList(Pos).initCapacity(gpa(), 1024*4);
    var lines_it = mem.splitScalar(u8, input, '\n');
    while (lines_it.next()) |line| {
        if (line.len == 0) continue;
        const pos = try Pos.fromStr(line);
        try bytes_al.append(pos);
    }
    const bytes = bytes_al.items;
    var left: usize = 0;
    var right: usize = bytes.len-1;
    while (left < right) {
        const mid = left + ((right-left)/2);
        const has = try hasExit(bytes, mid, map, grid_size);
        dprint("testing [{d} - {d} - {d}]: {any}\n", .{ left, mid, right, has });
        if (has) {
            left = mid+1;
        } else {
            right = mid;
        }
    }
    return .{ bytes[left-1].ci, bytes[left-1].ri };
}

fn test_solve(expected: [2]usize, grid_size: usize, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqualSlices(usize, &expected, &(try solve(grid_size, file.reader())));
}
test "example1" { try test_solve(.{6,1}, 7, "./18/example1.txt"); }
//test "input"    { try test_solve(.{0,0}, 71, "./18/input.txt"); }
