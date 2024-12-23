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
    counts: [][]usize,
    stack: std.ArrayList(State),

    const State = struct {
        pos: Pos,
        count: usize,

        pub fn init(pos: Pos, count: usize) State {
            return .{ .pos = pos, .count = count, };
        }
    };

    pub const start = 'S';
    pub const end   = 'E';
    const wall  = '#';

    const init_count = math.maxInt(usize);

    const dirs = [_][2]i8 {
        .{  1,  0 }, // up
        .{  0,  1 }, // right
        .{ -1,  0 }, // down
        .{  0, -1 }, // left
    };

    pub fn init(rows: [][] u8) !Map {
        var counts = try gpa().alloc([]usize, rows.len);
        for (0..counts.len) |i| { counts[i] = try gpa().alloc(usize, rows[i].len); }
        const stack = try std.ArrayList(State).initCapacity(gpa(), rows.len*rows[0].len*2);
        return .{
            .rows   = rows,
            .counts = counts,
            .stack  = stack,
        };
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

    pub fn countPath(self: *Map, from: Pos, to: Pos) !?usize {
        for (self.counts) |row| {
            for (row) |*count| {
                count.* = init_count;
            }
        }
        self.stack.clearRetainingCapacity();
        try self.stack.append(State.init(from, 0));
        while (self.stack.popOrNull()) |state| {
            for (dirs) |dir| {
                if (state.pos.offset(dir)) |nextpos| {
                    const obj = self.rows[nextpos.ri][nextpos.ci];
                    if (obj == wall) continue;
                    const nextcount = state.count + 1;
                    const count = self.counts[nextpos.ri][nextpos.ci];
                    if (count < nextcount) continue;
                    self.counts[nextpos.ri][nextpos.ci] = nextcount;
                    if (nextpos.eql(to)) continue;
                    try self.stack.append(State.init(nextpos, nextcount));
                }
            }
        }
        const to_count = self.counts[to.ri][to.ci];
        if (to_count == init_count) return null;
        return to_count;
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
    const start = try map.findObjPos(Map.start);
    const end   = try map.findObjPos(Map.end);
    const baseCount = try map.countPath(start, end) orelse return error.BaseCountNotFound;
    //dprint("base: {d}\n", .{baseCount});
    var total: usize = 0;
    // just trying out all possibilities - slow but works
    for (1..map.rows.len-1) |ri| {
        for (1..map.rows[0].len-1) |ci| {
            if (map.rows[ri][ci] != Map.wall) continue;
            map.rows[ri][ci] = '.';
            const count = try map.countPath(start, end) orelse return error.CountNotFound;
            map.rows[ri][ci] = Map.wall;
            if (count < baseCount) {
                const saved = baseCount - count;
                //dprint("saved: {d}\n", .{saved});
                if (saved >= threshold) total += 1;
            }
        }
    }
    return total;
}

fn test_solve(threshold: usize, expected: usize, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(threshold, file.reader()));
}
test { try test_solve(2,  14+30, "./20/example1.txt"); }
test { try test_solve(4,  14+16, "./20/example1.txt"); }
test { try test_solve(6,  2+14,  "./20/example1.txt"); }
test { try test_solve(8,  4+10,  "./20/example1.txt"); }
test { try test_solve(10, 2+8,   "./20/example1.txt"); }
test { try test_solve(12, 3+5,   "./20/example1.txt"); }
test { try test_solve(20, 1+4,   "./20/example1.txt"); }
test { try test_solve(36, 1+3,   "./20/example1.txt"); }
test { try test_solve(38, 1+2,   "./20/example1.txt"); }
test { try test_solve(40, 1+1,   "./20/example1.txt"); }
test { try test_solve(64, 1+0,   "./20/example1.txt"); }
// slow but works
//test "input" { try test_solve(100, 1518, "./20/input.txt"); }
