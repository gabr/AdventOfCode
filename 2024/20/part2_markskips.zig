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
    //try stdout.print("{d}\n", .{try solve(76, stdin)});
}

// singleton global general purpose allocator
fn gpa() Allocator {
    const GPA = std.heap.GeneralPurposeAllocator(.{});
    const S = struct { var gpa: ?GPA = null; };
    if (S.gpa == null) { S.gpa = GPA{}; }
    return S.gpa.?.allocator();
}

const Map = struct {
    rows:    [][]u8,
    path:    [][]usize,
    skips:   [][]usize,
    visited: [][]usize,

    pub const start_obj        = 'S';
    pub const end_obj          = 'E';
    pub const path_obj         = '.';
    pub const wall_obj         = '#';

    const dirs = [_][2]i8 {
        .{  1,  0 }, // up
        .{  0,  1 }, // right
        .{ -1,  0 }, // down
        .{  0, -1 }, // left
    };

    pub fn init(rows: [][] u8) !Map {
        return .{
            .rows    = rows,
            .path    = try initMatrix(usize, rows.len, rows[0].len, math.maxInt(usize)),
            .skips   = try initMatrix(usize, rows.len, rows[0].len, math.maxInt(usize)),
            .visited = try initMatrix(usize, rows.len, rows[0].len, 0),
        };
    }

    fn initMatrix(comptime T: type, rows: usize, columns: usize, default: T) ![][]T {
        var matrix = try gpa().alloc([]T, rows);
        for (0..rows) |ri| {
            matrix[ri] = try gpa().alloc(T, columns);
            for (0..columns) |ci| {
                matrix[ri][ci] = default;
            }
        }
        return matrix;
    }

    pub fn findObjPos(self: Map, obj: u8) !Pos {
        for (self.rows, 0..) |row, ri| {
            for (row, 0..) |o, ci| {
                if (o == obj) return Pos.init(ri, ci);
            }
        }
        dprint("Searching for: '{c}' failed, not found in map:\n", .{obj});
        for (self.rows) |row| { dprint("{s}\n", .{row}); }
        return error.NotFound;
    }

    pub fn findPath(self: *Map) !void {
        var   pos = try self.findObjPos(start_obj);
        var  prev = pos;
        const end = try self.findObjPos(end_obj);
        for (self.path) |row| { for (row) |*obj| { obj.* = math.maxInt(usize); } }
        self.path[pos.ri][pos.ci] = 0;
        while (!pos.eql(end)) {
            for (dirs) |dir| {
                if (pos.offset(dir)) |nextpos| {
                    if (!self.isPosInBounds(nextpos)) continue;
                    if (nextpos.eql(prev)) continue;
                    const nextobj = self.rows[nextpos.ri][nextpos.ci];
                    if (nextobj == wall_obj) continue;
                    self.path[nextpos.ri][nextpos.ci] = self.path[pos.ri][pos.ci] + 1;
                    prev = pos;
                    pos = nextpos;
                    break;
                }
            }
        }
        dprint("Path:\n", .{});
        for (self.path, 0..) |row, ri| {
            for (row, 0..) |p, ci| {
                if (p == math.maxInt(usize)) { dprint("{d: >2}x{d: <2} ", .{ri, ci}); }
                else                         { dprint("{d: ^5} ", .{p}); }
            }
            dprint("\n", .{});
        }
    }

    fn markSkips(self: *Map, from: Pos, to: Pos, at: Pos, count_above_saved: usize, steps: usize) !void {
        if (steps > 20) return;
        //dprint("testing (from: {d}x{d}, to: {d}x{d}, at: {d}x{d}, count_above_saved: {d}, steps: {d})\n", .{
        //    from.ri,from.ci,  to.ri,to.ci,  at.ri,at.ci,  count_above_saved, steps
        //});
        if (self.visited[at.ri][at.ci] > 0) return;
        self.visited[at.ri][at.ci] += 1;
        defer self.visited[at.ri][at.ci] -= 1;
        const obj = self.rows[at.ri][at.ci];
        if (obj != wall_obj) {
            blk: {
                const ps = self.path[from.ri][from.ci];
                const pe = self.path[at.ri][at.ci];
                assert(ps != math.maxInt(usize));
                assert(pe != math.maxInt(usize));
                if (ps > pe) break :blk;
                const diff = pe - ps;
                if (steps >= diff) break :blk;
                const saved = diff - steps;
                if (saved < count_above_saved) break :blk;
                const skip = self.skips[at.ri][at.ci];
                if (saved >= skip) break :blk;
                dprint("ps: {d}, pe: {d}, diff: {d}, steps: {d}, saved: {d} >= {d} (skip: {d})\n", .{
                    ps, pe, diff, steps, saved, count_above_saved, skip
                });
                self.skips[at.ri][at.ci] = saved;
                self.visited[at.ri][at.ci] += 1;
            }
            return;
        }
        for (dirs) |dir| {
            if (at.offset(dir)) |nextpos| {
                if (!self.isPosInBoundsOrWall(nextpos)) continue;
                if (self.visited[nextpos.ri][nextpos.ci] > 0) continue;
                try self.markSkips(from, to, nextpos, count_above_saved, steps + 1);
            }
        }
    }

    fn resetMatrix(comptime T: type, matrix: [][]T, reset_val: T) void {
        for (matrix) |row| {
            for (row) |*val| {
                val.* = reset_val;
            }
        }
    }

    fn countSkips(self: *Map) usize {
        var count: usize = 0;
        for (0..self.skips.len) |ri| {
            for (0..self.skips[0].len) |ci| {
                const skip = self.skips[ri][ci];
                const obj  = self.rows[ri][ci];
                if (obj != wall_obj and skip != math.maxInt(usize)) count += 1;
            }
        }
        return count;
    }

    pub fn countBelow(self: *Map, count_above_saved: usize) !usize {
        // find base path and mark it
        try self.findPath();
        // walk the path and test each wall
        const start = try self.findObjPos(start_obj);
        const end   = try self.findObjPos(end_obj);
        const pend  = self.path[end.ri][end.ci];
        var   pos   = start;
        var count: usize = 0;
        while (!pos.eql(end)) {
            const p = self.path[pos.ri][pos.ci];
            if (p > (pend-count_above_saved)) break;
            resetMatrix(usize, self.skips, math.maxInt(usize));
            for (dirs) |dir| {
                if (pos.offset(dir)) |nextpos| {
                    if (!self.isPosInBounds(nextpos)) continue;
                    const nextobj = self.rows[nextpos.ri][nextpos.ci];
                    if (nextobj == wall_obj) {
                        resetMatrix(usize, self.visited, 0);
                        try self.markSkips(pos, end, nextpos, count_above_saved, 1);
                    }
                }
            }
            const skips = self.countSkips();
            if (skips > 0) {
                dprint("from {d} ({d}x{d}) skips: {d}\n", .{self.path[pos.ri][pos.ci], pos.ri, pos.ci, skips});
            }
            count += skips;
            for (dirs) |dir| {
                if (pos.offset(dir)) |nextpos| {
                    if (!self.isPosInBounds(nextpos)) continue;
                    const np = self.path[nextpos.ri][nextpos.ci];
                    if (p + 1 != np) continue;
                    pos = nextpos;
                    break;
                }
            }
        }
        return count;
    }

    fn isPosInBoundsOrWall(self: Map, pos: Pos) bool {
        return pos.ri >= 0 and pos.ri <= self.rows.len-1 and
               pos.ci >= 0 and pos.ci <= self.rows[0].len-1;
    }

    fn isPosInBounds(self: Map, pos: Pos) bool {
        return pos.ri > 0 and pos.ri < self.rows.len-1 and
               pos.ci > 0 and pos.ci < self.rows[0].len-1;
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
    dprint("debug\n", .{});
    map = try Map.init(rows_al.items);
    return map.countBelow(threshold);
}

fn test_solve(threshold: usize, expected: usize, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(threshold, file.reader()));
}

//test { try test_solve(50, 32 + 255, "./20/example1.txt"); }
//test { try test_solve(52, 31 + 224, "./20/example1.txt"); }
//test { try test_solve(54, 29 + 195, "./20/example1.txt"); }
//test { try test_solve(56, 39 + 156, "./20/example1.txt"); }
//test { try test_solve(58, 25 + 131, "./20/example1.txt"); }
//test { try test_solve(60, 23 + 108, "./20/example1.txt"); }
//test { try test_solve(62, 20 +  88, "./20/example1.txt"); }
//test { try test_solve(64, 19 +  67, "./20/example1.txt"); }
//test { try test_solve(66, 12 +  55, "./20/example1.txt"); }
//test { try test_solve(68, 14 +  41, "./20/example1.txt"); }
//test { try test_solve(70, 12 +  29, "./20/example1.txt"); }
test { try test_solve(72, 22 +   7, "./20/example1.txt"); }
//test { try test_solve(74,  4 +   3, "./20/example1.txt"); }
//test { try test_solve(76,  3 +   0, "./20/example1.txt"); }

//test "input" { try test_solve(0, 0, "./20/input.txt"); }
// too high: 25899922
// too low:    231810
