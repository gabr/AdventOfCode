// NOTE: Correct calculations but not for this task X___x
// Takes into account only the shortest paths which is incorrect.
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
    rows:      [][]u8,
    counts:    [][]usize,
    stack:     std.ArrayList(State),
    exits:     std.ArrayList(Pos),
    entrances: std.ArrayList(Pos),

    pub const State = struct {
        pos: Pos,
        count: usize,
        inwall: usize,
        entrance: ?Pos = null,
        exit: ?Pos = null,

        pub fn init(pos: Pos, count: usize, inwall: usize) State {
            return .{ .pos = pos, .count = count, .inwall = inwall };
        }
    };

    pub const start_obj = 'S';
    pub const end_obj   = 'E';
    pub const path_obj  = '.';
    pub const wall_obj  = '#';


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
        const stack     = try std.ArrayList(State).initCapacity(gpa(), rows.len*rows[0].len*2);
        const exits     = try std.ArrayList(Pos)  .initCapacity(gpa(), rows.len*rows[1].len*2);
        const entrances = try std.ArrayList(Pos)  .initCapacity(gpa(), rows.len*rows[0].len*2);
        return .{
            .rows      = rows,
            .counts    = counts,
            .stack     = stack,
            .exits     = exits,
            .entrances = entrances,
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

    pub fn resetCounts(self: *Map) void {
        for (self.counts) |row| {
            for (row) |*count| {
                count.* = init_count;
            }
        }
    }

    pub fn findPath(self: *Map, start: Pos, end: Pos, wall_after_count: usize) !?usize {
        self.resetCounts();
        self.stack.clearRetainingCapacity();
        if (wall_after_count == 0) { try self.stack.append(State.init(start, 0, 20)); }
        else                       { try self.stack.append(State.init(start, 0, 0));  }
        self.counts[start.ri][start.ci] = 0;
        while (self.stack.popOrNull()) |state| {
            for (dirs) |dir| {
                if (state.pos.offset(dir)) |nextpos| {
                    if (!self.isPosInBounds(nextpos)) continue;
                    const obj = self.rows[nextpos.ri][nextpos.ci];
                    if (obj == wall_obj and state.inwall == 0) continue;
                    const nextcount = state.count + 1;
                    const count = self.counts[nextpos.ri][nextpos.ci];
                    if (count < nextcount) continue;
                    //const end_count = self.counts[end.ri][end.ci];
                    //if (end_count != init_count and count > end_count) continue;
                    self.counts[nextpos.ri][nextpos.ci] = nextcount;
                    if (nextpos.eql(end)) continue;
                    const nextobj = self.rows[nextpos.ri][nextpos.ci];
                    var nextinwall = state.inwall;
                    if (nextinwall > 0) {
                        if (nextobj == wall_obj)  { nextinwall -= 1; }
                        else if (obj == wall_obj) { nextinwall  = 0; }
                    }
                    else if (nextcount == wall_after_count) {
                        nextinwall = 20;
                    }
                    try self.stack.append(State.init(nextpos, nextcount, nextinwall));
                }
            }
        }
        const to_count = self.counts[end.ri][end.ci];
        if (to_count == init_count) return null;
        return to_count;
    }

    pub fn countSkips(self: *Map) !usize {
        self.stack.clearRetainingCapacity();
        self.exits.clearRetainingCapacity();
        self.entrances.clearRetainingCapacity();
        const endpos = try map.findObjPos(end_obj);
        const endcount = self.counts[endpos.ri][endpos.ci];
        assert(endcount != init_count);
        try self.stack.append(State.init(endpos, endcount, 0));
        while (self.stack.popOrNull()) |state| {
            for (dirs) |dir| {
                if (state.pos.offset(dir)) |nextpos| {
                    if (!self.isPosInBounds(nextpos)) continue;
                    const count = self.counts[nextpos.ri][nextpos.ci];
                    if (count == init_count) continue;
                    const nextcount = state.count - 1;
                    if (nextcount != count) continue;
                    const obj     = self.rows[state.pos.ri][state.pos.ci];
                    const nextobj = self.rows[nextpos.ri][nextpos.ci];
                    var nextstate = State.init(nextpos, nextcount, 0);
                    nextstate.entrance = state.entrance;
                    nextstate.exit = state.exit;
                    if (obj != wall_obj and nextobj == wall_obj) {
                        if (state.exit == null) { nextstate.exit = state.pos; }
                        else                    { continue; }
                    }
                    if (state.entrance == null and obj == wall_obj and nextobj != wall_obj) {
                        nextstate.entrance = nextpos;
                    }
                    if (nextcount == 0) {
                        if (nextstate.entrance) |pos| { try self.entrances.append(pos); }
                        if (nextstate.exit)     |pos| { try self.exits.append(pos);     }
                        continue;
                    }
                    try self.stack.append(nextstate);
                }
            }
        }
        //dprint("entrances: {any}\n", .{self.entrances.items});
        //dprint("exits: {any}\n", .{self.exits.items});
        const entrances_count = countDistinct(self.entrances.items);
        const exits_count = countDistinct(self.exits.items);
        //dprint("distinct entrances: {d}, exits: {d}\n", .{entrances_count, exits_count});
        return entrances_count * exits_count;
    }

    fn countDistinct(items: []Pos) usize {
        std.mem.sort(Pos, items, {}, sortPosFn);
        var prev = items[0];
        var count: usize = 1;
        for (items[1..]) |i| {
            if (i.eql(prev)) continue;
            count += 1;
            prev = i;
        }
        return count;
    }

    fn sortPosFn(context: void, lhs: Pos, rhs: Pos) bool {
        _ = context;
        return std.math.order(lhs.ri+lhs.ci*1000, rhs.ri+rhs.ci*1000) == .lt;
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
    map = try Map.init(rows_al.items);
    const start = try map.findObjPos(Map.start_obj);
    const end   = try map.findObjPos(Map.end_obj);
    map.resetCounts();
    const baseCount = try map.findPath(start, end, math.maxInt(usize)) orelse return error.BasePathNotFound;
    //dprint("baseCount: {d}\n", .{baseCount});
    var total: usize = 0;
    for (0..baseCount) |i| {
        const count = try map.findPath(start, end, i) orelse continue;
        if (i == 1) {
            for (map.counts) |row| {
                for (row) |co| {
                    if (co == Map.init_count) { dprint("{d: >6}", .{-1}); }
                    else                      { dprint("{d: >6}", .{co}); }
                }
                dprint("\n", .{});
            }
        }
        //return total;
        if (baseCount - count >= threshold) {
            const skips = try map.countSkips();
            dprint("{d}: count: {d}, skips: {d}\n", .{i, count, skips});
            total += skips;
        } else { break; }
    }
    return total;
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
//test { try test_solve(72, 22 +   7, "./20/example1.txt"); }
test { try test_solve(74,  4 +   3, "./20/example1.txt"); }
test { try test_solve(76,  3 +   0, "./20/example1.txt"); }

//test "input" { try test_solve(0, 0, "./20/input.txt"); }
