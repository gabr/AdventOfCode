const std = @import("std");
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

var rows: [][]u8 = undefined;
var starting_pos: [2]usize = undefined;
const Direction = enum { up, down, left, right, };
const Visited = struct {
    pos: [2]usize,
    dir: Direction,
};
var visited_pos: std.ArrayList(Visited) = undefined;
fn posAlreadyVisited(pos: [2]usize, dir: Direction) bool {
    for (visited_pos.items) |visited| {
        if (visited.dir == dir and
            visited.pos[0] == pos[0] and
            visited.pos[1] == pos[1]) {
            return true;
        }
    }
    return false;
}

fn nextPos(pos: [2]usize, dir: Direction) ?[2]usize {
    switch (dir) {
        .up    => {
            if (pos[0] == 0) return null;
            return .{pos[0]-1, pos[1]};
        },
        .down  => {
            if (pos[0] == rows.len-1) return null;
            return .{pos[0]+1, pos[1]};
        },
        .left  => {
            if (pos[1] == 0) return null;
            return .{pos[0], pos[1]-1};
        },
        .right => {
            if (pos[1] == rows[0].len-1) return null;
            return .{pos[0], pos[1]+1};
        },
    }
}

fn move(pos: *[2]usize, dir: *Direction) !bool {
    const start_dir = dir.*;
    while (nextPos(pos.*, dir.*)) |np| {
        if (rows[np[0]][np[1]]!='#') {
            pos.* = np;
            return true;
        }
        dir.* = switch (dir.*) {
            .up    => .right,
            .right => .down,
            .down  => .left,
            .left  => .up,
        };
        if (dir.* == start_dir) return error.ImpossibleToMove;
    }
    return false;
}

fn isPosStartingPos(pos: [2]usize) bool {
    return pos[0] == starting_pos[0] and
           pos[1] == starting_pos[1];
}

// returns true if end, false if loop
fn simulateTillTheEndOrLoop(start_pos: [2]usize, start_dir: Direction) !bool {
    var pos = start_pos;
    var dir = start_dir;
    visited_pos.items.len = 0;
    while (true) {
        if (posAlreadyVisited(pos, dir)) return false;
        try visited_pos.append(.{ .pos = pos, .dir = dir });
        if (!try move(&pos, &dir)) return true; // went outside of the board - the end
    }
    unreachable;
}

fn hasPos(pos: [2]usize, positions: std.ArrayList([2]usize)) bool {
    for (positions.items) |p| {
        if (p[0] == pos[0] and
            p[1] == pos[1]) return true;
    }
    return false;
}

fn solve(reader: anytype) !u64 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    var line_it = std.mem.splitAny(u8, input_al.items, "\n");
    var lines_al = try std.ArrayList([]u8).initCapacity(allocator, 1014);
    while(line_it.next()) |line| {
        if (line.len == 0) continue;
        try lines_al.append(@constCast(line));
    }
    rows = lines_al.items;
    // find starting position - direction is always up
    //var dir = Direction.up;
    var pos: [2]usize = undefined;
    for (rows,0..) |row,ri| {
        for (row,0..) |c,ci| {
            if (c == '^') {
                pos[0] = @intCast(ri);
                pos[1] = @intCast(ci);
                break;
            }
        }
    }
    starting_pos = pos;
    visited_pos = try std.ArrayList(Visited).initCapacity(allocator, 1024*16);
    var obstructions = try std.ArrayList([2]usize).initCapacity(allocator, 1024);
    for (rows,0..) |row,ri| {
        for (row,0..) |_,ci| {
            pos = .{ri,ci};
            if (isPosStartingPos(pos)) continue;
            if (rows[pos[0]][pos[1]] == '#') continue;
            rows[pos[0]][pos[1]] = '#';
            if (!try simulateTillTheEndOrLoop(starting_pos, Direction.up)) {
                if (!hasPos(pos, obstructions)) {
                    try obstructions.append(pos);
                }
            }
            rows[pos[0]][pos[1]] = '.';
        }
    }
    // TODO(arek): This is not correct but why? produces to big results
    //while (true) {
    //    const start_pos = pos;
    //    const start_dir = dir;
    //    if (!try move(&pos, &dir)) break; // out of the board
    //    if (!isPosStartingPos(pos)) {
    //        if (rows[pos[0]][pos[1]] != '#' and !hasPos(pos, obstructions)) {
    //            rows[pos[0]][pos[1]] = '#';
    //            if (!try simulateTillTheEndOrLoop(start_pos, start_dir)) {
    //                if (!hasPos(pos, obstructions)) {
    //                    try obstructions.append(pos);
    //                }
    //            }
    //            rows[pos[0]][pos[1]] = '.';
    //        }
    //    }
    //}
    //dprint("obstructions:\n", .{});
    //for (obstructions.items, 0..) |obs,i| {
    //    dprint("{d}: {any}\n", .{i,obs});
    //}
    return obstructions.items.len;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "06b example.a.txt" { try test_solve(6,  "./06/example.a.txt"); }
test "06b input.txt"     { try test_solve(1951, "./06/input.txt"); }
// too high: 2138, 2160
// incorrect: 2048, 2049, 2050, 2108, 2015

