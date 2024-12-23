const std = @import("std");
const math = std.math;
const mem = std.mem; const Allocator = std.mem.Allocator; const assert = std.debug.assert;
const isWhitespace = std.ascii.isWhitespace;
const dprint = std.debug.print;
//fn dprint(comptime fmt: []const u8, args: anytype) void { _=fmt; _=args; }

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(100, stdin)});
}

// singleton global general purpose allocator
fn gpa() Allocator {
    const GPA = std.heap.GeneralPurposeAllocator(.{});
    const S = struct { var gpa: ?GPA = null; };
    if (S.gpa == null) { S.gpa = GPA{}; }
    return S.gpa.?.allocator();
}

const Map = struct {
    rows: [][]u8,
    path: std.ArrayList(Pos),

    pub const start_obj = 'S';
    pub const end_obj   = 'E';
    pub const path_obj  = '.';
    pub const wall_obj  = '#';

    const dirs = [_][2]i8 {
        .{  1,  0 }, // up
        .{  0,  1 }, // right
        .{ -1,  0 }, // down
        .{  0, -1 }, // left
    };

    pub fn init(rows: [][] u8) !Map {
        return .{
            .rows = rows,
            .path = try std.ArrayList(Pos).initCapacity(gpa(), 1024),
        };
    }

    pub fn findObjPos(self: Map, obj: u8) !Pos {
        for (0..self.rows.len) |ri| {
            for (0..self.rows[0].len) |ci| {
                const o = self.rows[ri][ci];
                if (o == obj) return Pos.init(ri, ci);
            }
        }
        dprint("Searching for: '{c}' failed, not found in map:\n", .{obj});
        for (self.rows) |row| { dprint("{s}\n", .{row}); }
        return error.NotFound;
    }

    pub fn findPath(self: *Map) !void {
        self.path.clearRetainingCapacity();
        var   pos = try self.findObjPos(start_obj);
        var  prev = pos;
        const end = try self.findObjPos(end_obj);
        try self.path.append(pos);
        while (!pos.eql(end)) {
            for (dirs) |dir| {
                if (pos.offset(dir)) |nextpos| {
                    if (nextpos.eql(prev)) continue;
                    const nextobj = self.rows[nextpos.ri][nextpos.ci];
                    if (nextobj == wall_obj) continue;
                    prev = pos;
                    pos = nextpos;
                    try self.path.append(pos);
                    break;
                }
            }
        }
    }

    pub fn countBelow(self: *Map, count_above_saved: usize) !usize {
        // find base path and mark it
        try self.findPath();
        // walk the path and count possible skips
        var count: usize = 0;
        for (self.path.items, 0..) |p1, p1i| {
            for (self.path.items[p1i+1..], (p1i+1)..) |p2, p2i| {
                const skip = @abs(@as(isize,@intCast(p1.ri))-@as(isize,@intCast(p2.ri))) +
                             @abs(@as(isize,@intCast(p1.ci))-@as(isize,@intCast(p2.ci)));
                if (skip > 20) continue;
                const dist = p2i - p1i;
                if (skip > dist) continue;
                const saved = dist - skip;
                //dprint("from: {d}x{d} ({d}),  to: {d}x{d} ({d}), skip: {d}, dist: {d}, saved: {d}\n", .{
                //    p1.ri, p1.ci, p1i,
                //    p2.ri, p2.ci, p2i,
                //    skip,
                //    dist,
                //    saved,
                //});
                if (saved >= count_above_saved) {
                    count += 1;
                }
            }
        }
        return count;
    }
};

var map: Map = undefined;

const Pos = struct {
    ri: usize,
    ci: usize,

    pub fn init(ri: usize, ci: usize) Pos {
        return .{ .ri = ri, .ci = ci, };
    }

    pub fn offset(self: Pos, d: [2]i8) ?Pos {
        if ((d[0] < 0 and @abs(d[0]) > self.ri) or
            (d[1] < 0 and @abs(d[1]) > self.ci))
            return null;
        return Pos.init(
            @as(usize, @intCast( @as(isize, @intCast(self.ri)) + @as(isize, @intCast(d[0])) )),
            @as(usize, @intCast( @as(isize, @intCast(self.ci)) + @as(isize, @intCast(d[1])) )),
        );
    }

    pub fn eql(self: Pos, other: Pos) bool {
        return self.ri == other.ri and
               self.ci == other.ci;
    }
};

fn findExit(path: []Map.State) Pos {
    for (path) |state| {
        const obj = map.rows[state.pos.ri][state.pos.ci];
        if (obj != Map.wall_obj) return state.pos;
    }
    unreachable;
}

fn solve(threshold: usize, reader: anytype) !usize {
    const input = try reader.readAllAlloc(gpa(), std.math.maxInt(usize));
    var lines_it = mem.splitScalar(u8, input, '\n');
    var rows_al = try std.ArrayList([]u8).initCapacity(gpa(), 1024);
    while(lines_it.next()) |line_to_trim| {
        const line = std.mem.trim(u8, line_to_trim, "\r \t");
        if (line.len == 0) break;
        try rows_al.append(@constCast(line));
    }
    map = try Map.init(rows_al.items);
    return map.countBelow(threshold);
}

fn test_solve(threshold: usize, expected: usize, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(threshold, file.reader()));
}

test { try test_solve(50, 32 + 253, "./20/example1.txt"); }
test { try test_solve(52, 31 + 222, "./20/example1.txt"); }
test { try test_solve(54, 29 + 193, "./20/example1.txt"); }
test { try test_solve(56, 39 + 154, "./20/example1.txt"); }
test { try test_solve(58, 25 + 129, "./20/example1.txt"); }
test { try test_solve(60, 23 + 106, "./20/example1.txt"); }
test { try test_solve(62, 20 +  86, "./20/example1.txt"); }
test { try test_solve(64, 19 +  67, "./20/example1.txt"); }
test { try test_solve(66, 12 +  55, "./20/example1.txt"); }
test { try test_solve(68, 14 +  41, "./20/example1.txt"); }
test { try test_solve(70, 12 +  29, "./20/example1.txt"); }
test { try test_solve(72, 22 +   7, "./20/example1.txt"); }
test { try test_solve(74,  4 +   3, "./20/example1.txt"); }
test { try test_solve(76,  3 +   0, "./20/example1.txt"); }

test "input" { try test_solve(100, 1032257, "./20/input.txt"); }
// too high: 25899922
// too low:    231810
