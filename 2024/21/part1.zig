const std = @import("std");
const math = std.math;
const mem = std.mem; const Allocator = std.mem.Allocator; const assert = std.debug.assert;
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

fn parseInt(comptime T: type, str: []const u8) !T {
    return try std.fmt.parseInt(T, str, 10);
}

///    0   1   2    < ci
///  +---+---+---+
///  | 7 | 8 | 9 |  0
///  +---+---+---+
///  | 4 | 5 | 6 |  1
///  +---+---+---+
///  | 1 | 2 | 3 |  2
///  +---+---+---+
///      | 0 | A |  3
///      +---+---+
///                 ^
///                 ri
fn numKeypadPos(key: u8) [2]i8 { // .{ ri, ci }
    return switch (key) {
        '7' => .{0,0}, '8' => .{0,1}, '9' => .{0,2},
        '4' => .{1,0}, '5' => .{1,1}, '6' => .{1,2},
        '1' => .{2,0}, '2' => .{2,1}, '3' => .{2,2},
                       '0' => .{3,1}, 'A' => .{3,2},
        else => {
            dprint("Unknown key: '{c}'\n", .{key});
            unreachable;
        },
    };
}

///    0   1   2   < ci
///      +---+---+
///      | ^ | A | 0
///  +---+---+---+
///  | < | v | > | 1
///  +---+---+---+
///                ^
///                ri
fn dirKeypadPos(key: u8) [2]i8 { // .{ ri, ci }
    return switch (key) {
                        '^' =>  .{0,1}, 'A' =>  .{0,2},
        '<' =>  .{1,0}, 'v' =>  .{1,1}, '>' =>  .{1,2},
        else => {
            dprint("Unknown key: '{c}'\n", .{key});
            unreachable;
        },
    };
}

fn diffToMoves(diff: [2]i8, buf: []u8, updownfirst: bool) []u8 {
    var i: usize = 0;
    const updown    : u8 = if (diff[0] > 0) '^' else 'v';
    const rightleft : u8 = if (diff[1] < 0) '>' else '<';
    if (updownfirst) {
        for (0..@abs(diff[0])) |_| { buf[i] = updown;    i+=1; }
        for (0..@abs(diff[1])) |_| { buf[i] = rightleft; i+=1; }
    } else {
        for (0..@abs(diff[1])) |_| { buf[i] = rightleft; i+=1; }
        for (0..@abs(diff[0])) |_| { buf[i] = updown;    i+=1; }
    }
    return buf[0..i];
}

fn posDiff(p1: [2]i8, p2: [2]i8) [2]i8 {
    return .{ p1[0] - p2[0], p1[1] - p2[1] };
}

fn typeDirKeys(keys: []const u8, directional_keypads_count: u8) usize {
    var buf: [8]u8 = undefined;
    var prevpos = dirKeypadPos('A');
    var len: usize = 0;
    var up: [2]bool = undefined;
    var upi: usize = 0;
    for (keys) |key| {
        const nextpos = dirKeypadPos(key);
        const diff = posDiff(prevpos, nextpos);
             if (prevpos[1] == 0 and nextpos[0] == 0) { upi = 1; up[0] = false; }
        else if (prevpos[0] == 0 and nextpos[1] == 0) { upi = 1; up[0] = true;  }
        else                                          { upi = 2; up[0] = true; up[1] = false; }
        var min: usize = math.maxInt(usize);
        for (up) |updownfirst| {
            const seq = diffToMoves(diff, buf[0..], updownfirst);
            buf[seq.len] = 'A';
            const seqa = buf[0..seq.len+1];
            var dirlen: usize = 0;
            if (directional_keypads_count > 0) {
                dirlen = typeDirKeys(seqa, directional_keypads_count-1);
            }
            const total = seq.len + dirlen;
            if (total < min) {
                min = total;
                //for (0..(3-directional_keypads_count)) |_| { dprint(" ", .{}); }
                //dprint("new min: seq '{s}' ({d}), dirlen: {d}, total: {d}\n", .{seq, seq.len, dirlen, total});
            }
        }
        len += min;
        prevpos = nextpos;
    }
    return len;
}

fn typeKeys(code: []const u8, directional_keypads_count: u8) usize {
    dprint("debugging code: '{s}'\n", .{code});
    var buf: [8]u8 = undefined;
    var prevkey: u8 = 'A';
    var prevpos = numKeypadPos('A');
    var len: usize = 0;
    var up: [2]bool = undefined;
    var upi: usize = 0;
    for (code) |key| {
        const nextpos = numKeypadPos(key);
        const diff = posDiff(prevpos, nextpos);
             if (prevpos[1] == 0 and nextpos[0] == 3) { upi = 1; up[0] = false; }
        else if (prevpos[0] == 3 and nextpos[1] == 0) { upi = 1; up[0] = true;  }
        else                                          { upi = 2; up[0] = true; up[1] = false; }
        var min: usize = math.maxInt(usize);
        for (up[0..upi]) |updownfirst| {
            const seq = diffToMoves(diff, buf[0..], updownfirst);
            buf[seq.len] = 'A';
            const seqa = buf[0..seq.len+1];
            //dprint ("{c} ({d},{d}) -> {c} ({d}, {d}) '{s}' ({d})\n", .{
            //    prevkey, prevpos[0], prevpos[1],
            //        key, nextpos[0], nextpos[1],
            //    seqa, seqa.len });
            const dirlen = typeDirKeys(seqa, directional_keypads_count-1);
            const total = seqa.len + dirlen;
            if (total < min) {
                //dprint("new min: seqa '{s}' ({d}), dirlen: {d}, total: {d}\n", .{seqa, seqa.len, dirlen, total});
                min = total;
            }
        }
        len += min;
        prevpos = nextpos;
        prevkey = key;
    }
    return len;
}

fn solve(reader: anytype) !usize {
    const input = try reader.readAllAlloc(gpa(), std.math.maxInt(usize));
    var lines_it = mem.splitScalar(u8, input, '\n');
    var sum: usize = 0;
    while(lines_it.next()) |line_to_trim| {
        const line = std.mem.trim(u8, line_to_trim, "\r \t");
        if (line.len == 0) break;
        const len = typeKeys(line, 2);
        const number = try parseInt(usize, line[0..line.len-1]);
        sum += len * number;
    }
    return sum;
}

fn test_solve(expected: usize, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}

test { try std.testing.expectEqual("<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A" .len, typeKeys("029A", 2)); }
test { try std.testing.expectEqual("<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A"         .len, typeKeys("980A", 2)); }
test { try std.testing.expectEqual("<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" .len, typeKeys("179A", 2)); }
test { try std.testing.expectEqual("<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A"     .len, typeKeys("456A", 2)); }
test { try std.testing.expectEqual("<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"     .len, typeKeys("379A", 2)); }

test { try test_solve(126384, "./21/example1.txt"); }
test { try test_solve(94284, "./21/input.txt");    }
// too high: 98508
