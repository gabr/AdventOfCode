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

// singleton global general purpose allocator
fn gpa() Allocator {
    const GPA = std.heap.GeneralPurposeAllocator(.{});
    const S = struct { var gpa: ?GPA = null; };
    if (S.gpa == null) { S.gpa = GPA{}; }
    return S.gpa.?.allocator();
}

fn readMap(input: []const u8) ![][]u8 {
    var it = mem.tokenizeAny(u8, input, "\n\r");
    var al = try std.ArrayList([]u8).initCapacity(gpa(), 1024*16);
    while (it.next()) |line| { al.appendAssumeCapacity(@constCast(line)); }
    return al.items;
}

fn allocMap(comptime T: type, rows: usize, cols: usize) ![][]T {
    const map = try gpa().alloc([]T, rows);
    for (0..rows) |ri| {
        map[ri] = try gpa().alloc(T, cols);
    }
    return map;
}

fn resetMap(comptime T: type, map:[][]T, val: T) void {
    for (0..map.len) |ri| {
        for (0..map[0].len) |ci| {
            map[ri][ci] = val;
        }
    }
}

fn findObj(obj: u8, map: [][]u8) [2]usize {
    for (0..map.len) |ri| {
        for (0..map[0].len) |ci| {
            if (map[ri][ci]==obj) return .{ri,ci};
        }
    }
    unreachable;
}

fn getObj(map: [][]u8, pos: [2]usize) u8 {
    return map[pos[0]][pos[1]];
}

// TODO(arek): This is stupid, just have an array of offsets instead next time
//             like: .{ .{0,1}, .{-1,0}, .{0,-1}, .{1,0} }
const Dir = enum { E, S, W, N };
const dir_count = @typeInfo(Dir).Enum.fields.len;
const empty = '.';
const wall = '#';
const end = 'E';
const step_score = 1;
const turn_score = 1000;

const State = struct {
    pos: [2]usize,
    dir: Dir,
    score: usize,
};

fn nextPos(pos: [2]usize, dir: Dir) [2]usize {
    const offset = dirToOffset(dir);
    return .{
        @as(usize, @intCast(@as(isize, @intCast(pos[0])) + @as(isize, @intCast(offset[0])))),
        @as(usize, @intCast(@as(isize, @intCast(pos[1])) + @as(isize, @intCast(offset[1])))),
    };
}

fn dirToOffset(d: Dir) [2]i8 {
    switch (d) {
        Dir.E => return .{ 0, 1},
        Dir.S => return .{-1, 0},
        Dir.W => return .{ 0,-1},
        Dir.N => return .{ 1, 0},
    }
}

fn rotateDir(dir: Dir, by: i8) Dir {
    const d: i8 = @intFromEnum(dir);
    const m: i8 = @intCast(dir_count);
    const nd = @rem(@rem(d+by, m) + m, m);
    return @enumFromInt(@as(usize, @intCast(nd)));
}

fn countMinVisited(map: [][]u8, visited: [][]usize) !u64 {
    const start_pos = findObj(end, map);
    var stack = try std.ArrayList([2]usize).initCapacity(gpa(), 1024*16);
    try stack.append(start_pos);
    while (stack.popOrNull()) |pos| {
        map[pos[0]][pos[1]] = 'O';
        const score = visited[pos[0]][pos[1]];
        //dprint("score: {d} at: {any}\n", .{score, pos});
        if (score == 0) continue;
        for (0..dir_count) |dir| {
            const nextpos = nextPos(pos, @enumFromInt(dir));
            const nextscore = visited[nextpos[0]][nextpos[1]];
            if (nextscore == std.math.maxInt(u64)) continue;
            if (nextscore < score ) {
                //dprint("pos: {any} -> nextpos: {any} nextscore: {d}\n", .{pos, nextpos, nextscore});
                try stack.append(nextpos);
            }
        }
    }
    var count: u64 = 0;
    for (map) |row| {
        for (row) |obj| {
            if (obj == 'O') {
                count += 1;
            }
        }
    }
    for (map) |row| { dprint("{s}\n", .{row}); }
    return count;
}

fn findMinPath(map: [][]u8, start_pos: [2]usize, visited: [][]usize) !u64 {
    assert(map.len == visited.len);
    assert(map[0].len == visited[0].len);
    resetMap(usize, visited, std.math.maxInt(usize));
    var stack = try std.ArrayList(State).initCapacity(gpa(), 1024*16);
    try stack.append(.{ .pos = start_pos, .dir = Dir.E, .score = 0 });
    try stack.append(.{ .pos = start_pos, .dir = Dir.S, .score = turn_score});
    try stack.append(.{ .pos = start_pos, .dir = Dir.N, .score = turn_score});
    try stack.append(.{ .pos = start_pos, .dir = Dir.W, .score = turn_score*2});
    var min_score: u64 = std.math.maxInt(u64);
    while (stack.popOrNull()) |state| {
        const tmpscore = visited[state.pos[0]][state.pos[1]];
        if (visited[state.pos[0]][state.pos[1]] > state.score) {
            visited[state.pos[0]][state.pos[1]] = state.score;
        }
        const nextpos = nextPos(state.pos, state.dir);
        if (getObj(map, nextpos) == end) {
            if (state.score+step_score < min_score) {
                min_score = state.score+step_score;
                if (visited[nextpos[0]][nextpos[1]] > state.score+step_score) {
                    visited[nextpos[0]][nextpos[1]] = state.score+step_score;
                }
            }
            continue;
        }
        if (getObj(map, nextpos) != empty) {
            visited[state.pos[0]][state.pos[1]] = tmpscore;
            continue;
        }
        if (visited[nextpos[0]][nextpos[1]] < state.score) {
            visited[state.pos[0]][state.pos[1]] = tmpscore;
            continue;
        }
        try stack.append(.{ .pos = nextpos, .dir = rotateDir(state.dir,   1), .score = state.score+step_score+turn_score});
        try stack.append(.{ .pos = nextpos, .dir = rotateDir(state.dir,  -1), .score = state.score+step_score+turn_score});
        try stack.append(.{ .pos = nextpos, .dir = state.dir,                 .score = state.score+step_score});
    }
    for (visited) |row| {
        for (row) |v| {
            if (v == std.math.maxInt(u64)) {
                dprint("{d: >6}", .{-1});
            } else {
                dprint("{d: >6}", .{v});
            }
        }
        dprint("\n", .{});
    }
    return min_score;
}

fn solve(reader: anytype) !u64 {
    const input = try reader.readAllAlloc(gpa(), std.math.maxInt(usize));
    const map = try readMap(input);
    const visited = try allocMap(usize, map.len, map[0].len);
    const start_pos = findObj('S', map);
    dprint("debug:\n", .{});
    dprint("start pos: {any}\n", .{start_pos});
    _ = try findMinPath(map, start_pos, visited);
    return countMinVisited(map, visited);
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example1" { try test_solve(45,  "./16/example1.txt"); }
test "example2" { try test_solve(64,  "./16/example2.txt"); }
test "input"    { try test_solve(565, "./16/input.txt"); }
