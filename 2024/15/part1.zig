const std = @import("std");
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

fn alloc() Allocator {
    const Arena = std.heap.ArenaAllocator;
    const S = struct { var arena: ?Arena = null; };
    if (S.arena == null) { S.arena = Arena.init(std.heap.page_allocator); }
    return S.arena.?.allocator();
}

const robot = '@';
const empty = '.';
const box = 'O';

fn readMap(input: []const u8) ![][]u8 {
    var it = mem.tokenizeAny(u8, input, "\n\r");
    var al = try std.ArrayList([]u8).initCapacity(alloc(), 1024*16);
    while (it.next()) |line| { al.appendAssumeCapacity(@constCast(line)); }
    return al.items;
}

fn find(obj: u8, map: [][]u8) [2]usize {
    for (0..map.len) |ri| {
        for (0..map[0].len) |ci| {
            if (map[ri][ci]==obj) return .{ri,ci};
        }
    }
    unreachable;
}

fn moveToOffset(move: u8) [2]i8 {
    switch (move) {
        '^' => return .{-1, 0},
        '>' => return .{ 0, 1},
        'v' => return .{ 1, 0},
        '<' => return .{ 0,-1},
        else => unreachable,
    }
}

fn moveBy(pos: [2]usize, offset: [2]i8) [2]usize {
    return .{
        @as(usize, @intCast(@as(isize, @intCast(pos[0])) + @as(isize, @intCast(offset[0])))),
        @as(usize, @intCast(@as(isize, @intCast(pos[1])) + @as(isize, @intCast(offset[1])))),
    };
}

fn moveObj(pos: [2]usize, offset: [2]i8, map: [][]u8) void {
    const newpos = moveBy(pos, offset);
    const obj = map[newpos[0]][newpos[1]];
    if (obj != empty) {
        dprint("Not an empty space {any}->{any} ({any} '{c}'\n", .{pos, newpos, offset, obj});
        unreachable;
    }
    map[newpos[0]][newpos[1]] = map[pos[0]][pos[1]];
    map[pos[0]][pos[1]] = empty;
}

fn trymove(pos: [2]usize, offset: [2]i8, map: [][]u8) bool {
    const newpos = moveBy(pos, offset);
    const obj = map[newpos[0]][newpos[1]];
    dprint("trymove({any}, {any}) obj: '{c}'\n", .{pos, offset, obj});
    if (obj != empty and (obj != box or !trymove(newpos, offset, map))) {
        return false;
    }
    moveObj(pos, offset, map);
    return true;
}

fn solve(reader: anytype) !u64 {
    const input = try reader.readAllAlloc(alloc(), std.math.maxInt(usize));
    var empty_line_split = mem.splitSequence(u8, input, "\n\n");
    const map_str = empty_line_split.next().?;
    const map = try readMap(map_str);
    const moves = empty_line_split.next().?;
    var pos = find(robot, map);
    dprint("debug:\n", .{});
    dprint("init pos: {any}\n", .{pos});
    for (moves) |move| {
        if (isWhitespace(move)) continue;
        const offset = moveToOffset(move);
        if (trymove(pos, offset, map)) {
            pos = moveBy(pos, offset);
        }
    }
    var sum: usize = 0;
    for (0..map.len) |ri| {
        for (0..map[0].len) |ci| {
            const obj = map[ri][ci];
            if (obj != box) continue;
            sum += 100*ri+ci;
        }
    }
    return sum;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example0" { try test_solve(2028,    "./15/example0.txt"); }
test "example1" { try test_solve(10092,   "./15/example1.txt"); }
test "input"    { try test_solve(1463715, "./15/input.txt"); }
