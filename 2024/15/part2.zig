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
const box = "[]";
const wall = '#';
const empty = '.';

fn readMap(input: []const u8) ![][]u8 {
    var it = mem.tokenizeAny(u8, input, "\n\r");
    var al = try std.ArrayList([]u8).initCapacity(alloc(), 1024*16);
    while (it.next()) |line| { al.appendAssumeCapacity(@constCast(line)); }
    return al.items;
}

fn scaleUpMap(map: [][]u8) ![][]u8 {
    const sbox = 'O';
    const a = alloc();
    var new_map = try a.alloc([]u8, map.len);
    for (0..map.len) |ri| {
        new_map[ri] = try a.alloc(u8, map[ri].len*2);
        for (0..map[ri].len) |ci| {
            const obj = map[ri][ci];
            switch (obj) {
                empty => { new_map[ri][ci*2] = empty;  new_map[ri][(ci*2)+1] = empty;  },
                sbox  => { new_map[ri][ci*2] = box[0]; new_map[ri][(ci*2)+1] = box[1]; },
                wall  => { new_map[ri][ci*2] = wall;   new_map[ri][(ci*2)+1] = wall;   },
                robot => { new_map[ri][ci*2] = robot;  new_map[ri][(ci*2)+1] = empty;  },
                else => unreachable,
            }
        }
    }
    return new_map;
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

fn isBox(obj: u8) bool {
    return obj == box[0] or obj == box[1];
}

fn getBoxPosses(obj_pos: [2]usize, map: [][]u8) [2][2]usize {
    const obj = map[obj_pos[0]][obj_pos[1]];
    if (obj == box[0]) {
        return .{ obj_pos, moveBy(obj_pos, .{0, 1}), };
    }
    if (obj == box[1]) {
        return .{ moveBy(obj_pos, .{0,-1}), obj_pos };
    }
    unreachable; // not a box object
}

fn moveObj(pos: [2]usize, offset: [2]i8, map: [][]u8) void {
    const obj = map[pos[0]][pos[1]]; // obj to move
    dprint("movObj({any}, {any}) '{c}'\n", .{pos, offset, obj});
    if (obj == robot) {
        const newpos = moveBy(pos, offset);
        map[newpos[0]][newpos[1]] = obj;
        map[pos[0]][pos[1]] = empty;
        return;
    }
    if (isBox(obj)) {
        const boxpos = getBoxPosses(pos, map);
        map[boxpos[0][0]][boxpos[0][1]] = empty;
        map[boxpos[1][0]][boxpos[1][1]] = empty;
        const mboxpos = .{
            moveBy(boxpos[0], offset),
            moveBy(boxpos[1], offset),
        };
        map[mboxpos[0][0]][mboxpos[0][1]] = box[0];
        map[mboxpos[1][0]][mboxpos[1][1]] = box[1];
        return;
    }
    unreachable;
}

fn trymoveRobot(pos: [2]usize, offset: [2]i8, map: [][]u8) bool {
    dprint("trymoveRobot({any}, {any}) ", .{pos, offset});
    assert(map[pos[0]][pos[1]] == robot);
    const newpos = moveBy(pos, offset);
    const obj = map[newpos[0]][newpos[1]];
    dprint(" -> {any} '{c}'\n", .{newpos, obj});
    if (obj == wall) {
        return false;
    }
    if (obj == empty) {
        moveObj(pos, offset, map);
        return true;
    }
    if (isBox(obj)) {
        if (!canMoveBox(newpos, offset, map)) return false;
        assert(moveBox(newpos, offset, map));
        moveObj(pos, offset, map);
        return true;
    }
    unreachable;
}

fn canMoveBox(pos: [2]usize, offset: [2]i8, map: [][]u8) bool {
    dprint("canMoveBox({any}, {any}) ", .{pos, offset});
    const boxpos = getBoxPosses(pos, map);
    const mboxpos = .{
        moveBy(boxpos[0], offset),
        moveBy(boxpos[1], offset),
    };
    dprint("[{any}] -> [{any}] ", .{boxpos, mboxpos});
    if (offset[0] == 0) { // left right
        // right
        if (offset[1] > 0) {
            dprint("right", .{});
            const obj = map[mboxpos[1][0]][mboxpos[1][1]];
            if (obj != empty) {
                if (obj == wall or !canMoveBox(mboxpos[1], offset, map)) {
                    dprint(" - not empty\n", .{});
                    return false;
                }
            }
            dprint(" - empty\n", .{});
            return true;
        }
        // left
        else {
            dprint("left", .{});
            const obj = map[mboxpos[0][0]][mboxpos[0][1]];
            if (obj != empty) {
                if (obj == wall or !canMoveBox(mboxpos[0], offset, map)) {
                    dprint(" - not empty\n", .{});
                    return false;
                }
            }
            dprint(" - empty\n", .{});
            return true;
        }
    } else {
        dprint("top or down", .{});
        if (map[mboxpos[0][0]][mboxpos[0][1]] == empty and
            map[mboxpos[1][0]][mboxpos[1][1]] == empty) {
            dprint("- both empty\n", .{});
            return true;
        }
        if (map[mboxpos[0][0]][mboxpos[0][1]] == wall or
            map[mboxpos[1][0]][mboxpos[1][1]] == wall) {
            dprint("- both wall\n", .{});
            return false;
        }
        if (map[mboxpos[0][0]][mboxpos[0][1]] == box[0] and
            map[mboxpos[1][0]][mboxpos[1][1]] == box[1]) {
            dprint("- both box\n", .{});
            return canMoveBox(mboxpos[0], offset, map);
        }
        if (map[mboxpos[0][0]][mboxpos[0][1]] == box[1] or
            map[mboxpos[1][0]][mboxpos[1][1]] == box[0]) {
            dprint("- possibly two boxes\n", .{});
            if (map[mboxpos[0][0]][mboxpos[0][1]] == box[1]) {
                if (!canMoveBox(mboxpos[0], offset, map)) return false;
            }
            if (map[mboxpos[1][0]][mboxpos[1][1]] == box[0]) {
                if (!canMoveBox(mboxpos[1], offset, map)) return false;
            }
            return true;
        }
        dprint("- unknown\n", .{});
        for (map) |row| { dprint("{s}\n", .{row}); }
        unreachable;
    }
}

fn moveBox(pos: [2]usize, offset: [2]i8, map: [][]u8) bool {
    dprint("moveBox({any}, {any}) ", .{pos, offset});
    const boxpos = getBoxPosses(pos, map);
    const mboxpos = .{
        moveBy(boxpos[0], offset),
        moveBy(boxpos[1], offset),
    };
    dprint("[{any}] -> [{any}] ", .{boxpos, mboxpos});
    if (offset[0] == 0) { // left right
        // right
        if (offset[1] > 0) {
            dprint("right", .{});
            const obj = map[mboxpos[1][0]][mboxpos[1][1]];
            if (obj != empty) {
                if (obj == wall or !moveBox(mboxpos[1], offset, map)) {
                    dprint(" - not empty\n", .{});
                    return false;
                }
            }
            dprint(" - empty\n", .{});
            moveObj(pos, offset, map);
            return true;
        }
        // left
        else {
            dprint("left", .{});
            const obj = map[mboxpos[0][0]][mboxpos[0][1]];
            if (obj != empty) {
                if (obj == wall or !moveBox(mboxpos[0], offset, map)) {
                    dprint(" - not empty\n", .{});
                    return false;
                }
            }
            dprint(" - empty\n", .{});
            moveObj(pos, offset, map);
            return true;
        }
    } else {
        dprint("top or down", .{});
        if (map[mboxpos[0][0]][mboxpos[0][1]] == empty and
            map[mboxpos[1][0]][mboxpos[1][1]] == empty) {
            dprint("- both empty\n", .{});
            moveObj(pos, offset, map);
            return true;
        }
        if (map[mboxpos[0][0]][mboxpos[0][1]] == wall or
            map[mboxpos[1][0]][mboxpos[1][1]] == wall) {
            dprint("- both wall\n", .{});
            return false;
        }
        if (map[mboxpos[0][0]][mboxpos[0][1]] == box[0] and
            map[mboxpos[1][0]][mboxpos[1][1]] == box[1]) {
            dprint("- both box\n", .{});
            if (!moveBox(mboxpos[0], offset, map)) return false;
            moveObj(pos, offset, map);
            return true;
        }
        if (map[mboxpos[0][0]][mboxpos[0][1]] == box[1] or
            map[mboxpos[1][0]][mboxpos[1][1]] == box[0]) {
            dprint("- possibly two boxes\n", .{});
            if (map[mboxpos[0][0]][mboxpos[0][1]] == box[1]) {
                if (!moveBox(mboxpos[0], offset, map)) return false;
            }
            if (map[mboxpos[1][0]][mboxpos[1][1]] == box[0]) {
                if (!moveBox(mboxpos[1], offset, map)) return false;
            }
            moveObj(pos, offset, map);
            return true;
        }
        dprint("- unknown\n", .{});
        for (map) |row| { dprint("{s}\n", .{row}); }
        unreachable;
    }
}

fn solve(reader: anytype) !u64 {
    const input = try reader.readAllAlloc(alloc(), std.math.maxInt(usize));
    var empty_line_split = mem.splitSequence(u8, input, "\n\n");
    const small_map = try readMap(empty_line_split.next().?);
    const map = try scaleUpMap(small_map);
    const moves = empty_line_split.next().?;
    var pos = find(robot, map);
    dprint("debug:\n", .{});
    dprint("init pos: {any}\n", .{pos});
    for (map) |row| { dprint("{s}\n", .{row}); }
    for (moves) |move| {
        if (isWhitespace(move)) continue;
        dprint("move: '{c}'\n", .{move});
        const offset = moveToOffset(move);
        if (trymoveRobot(pos, offset, map)) {
            pos = moveBy(pos, offset);
        }
        dprint("map:\n", .{});
        for (map) |row| { dprint("{s}\n", .{row}); }
    }
    dprint("final map:\n", .{});
    for (map) |row| { dprint("{s}\n", .{row}); }
    var sum: usize = 0;
    for (0..map.len) |ri| {
        for (0..map[0].len) |ci| {
            const obj = map[ri][ci];
            if (obj != box[0]) continue;
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
test "example1" { try test_solve(9021,   "./15/example1.txt"); }
test "input"    { try test_solve(1481392, "./15/input.txt"); }
