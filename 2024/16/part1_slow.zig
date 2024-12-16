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
    const GPA = std.heap.GeneralPurposeAllocator(.{});
    const S = struct { var gpa: ?GPA = null; };
    if (S.gpa == null) { S.gpa = GPA{}; }
    return S.gpa.?.allocator();
}

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
    visited: std.ArrayList([2]usize),

    pub fn init(pos: [2]usize, dir: Dir, score: usize, visited: std.ArrayList([2]usize)) !State {
        var my_visited = visited;
        try my_visited.append(pos);
        return .{
            .pos     = pos,
            .dir     = dir,
            .score   = score,
            .visited = my_visited,
        };
    }

    pub fn wasVisited(self: State, pos: [2]usize) bool {
        for (self.visited.items) |vpos| {
            if (vpos[0] == pos[0] and vpos[1] == pos[1]) {
                return true;
            }
        }
        return false;
    }
};

fn getObj(map: [][]u8, pos: [2]usize) u8 {
    return map[pos[0]][pos[1]];
}


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

fn findMinPath(map: [][]u8, start_pos: [2]usize) !u64 {
    var stack = try std.ArrayList(State).initCapacity(alloc(), 1024*16);
    try stack.append(try State.init(start_pos, Dir.E, 0,            try std.ArrayList([2]usize).initCapacity(alloc(), 2)));
    try stack.append(try State.init(start_pos, Dir.S, turn_score,   try std.ArrayList([2]usize).initCapacity(alloc(), 2)));
    try stack.append(try State.init(start_pos, Dir.N, turn_score,   try std.ArrayList([2]usize).initCapacity(alloc(), 2)));
    try stack.append(try State.init(start_pos, Dir.W, turn_score*2, try std.ArrayList([2]usize).initCapacity(alloc(), 2)));
    var min_score: u64 = std.math.maxInt(u64);
    while (stack.popOrNull()) |state| {
        const nextpos = nextPos(state.pos, state.dir);
        if (getObj(map, nextpos) == end) {
            if (state.score+step_score < min_score) {
                min_score = state.score+step_score;
            }
            state.visited.deinit();
            continue;
        }
        if (getObj(map, nextpos) != empty) {
            state.visited.deinit();
            continue;
        }
        if (state.wasVisited(nextpos)) {
            state.visited.deinit();
            continue;
        }
        var visited_copy = try std.ArrayList([2]usize).initCapacity(alloc(), state.visited.items.len+2);
        try visited_copy.appendSlice(state.visited.items);
        try stack.append(try State.init(nextpos, rotateDir(state.dir,  1), state.score+step_score+turn_score, visited_copy));

        visited_copy = try std.ArrayList([2]usize).initCapacity(alloc(), state.visited.items.len+2);
        try visited_copy.appendSlice(state.visited.items);
        try stack.append(try State.init(nextpos, rotateDir(state.dir,  -1), state.score+step_score+turn_score, visited_copy));

        try stack.append(try State.init(nextpos, state.dir, state.score+step_score, state.visited));
    }
    return min_score;
}

fn solve(reader: anytype) !u64 {
    const input = try reader.readAllAlloc(alloc(), std.math.maxInt(usize));
    const map = try readMap(input);
    const start_pos = find('S', map);
    dprint("debug:\n", .{});
    dprint("start pos: {any}\n", .{start_pos});
    return try findMinPath(map, start_pos);
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example1" { try test_solve(7036,   "./16/example1.txt"); }
test "example2" { try test_solve(11048,  "./16/example2.txt"); }
//test "input"    { try test_solve(0, "./16/input.txt"); }
