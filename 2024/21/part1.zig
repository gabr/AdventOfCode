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

fn numKeypadPos(key: u8) [2]i8 { // .{row index, column index}
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

fn dirKeypadPos(key: u8) [2]i8 { // .{row index, column index}
    return switch (key) {
                        '^' =>  .{0,1}, 'A' =>  .{0,2},
        '<' =>  .{1,0}, 'v' =>  .{1,1}, '>' =>  .{1,2},
        else => {
            dprint("Unknown key: '{c}'\n", .{key});
            unreachable;
        },
    };
}

/// produces array of moves
fn diffToMoves(diff: [2]i8, buf: []u8, updownfirst: bool) []u8 {
    var i: usize = 0;
    const updown    : u8 = if (diff[0] > 0) '^' else 'v';
    const rightleft : u8 = if (diff[1] < 0) '>' else '<';
    var rc: u8 = @abs(diff[0]);
    var cc: u8 = @abs(diff[1]);
    if (updownfirst) {
        while (rc > 0) : (rc -= 1) { buf[i] = updown;    i += 1; }
        while (cc > 0) : (cc -= 1) { buf[i] = rightleft; i += 1; }
    } else {
        while (cc > 0) : (cc -= 1) { buf[i] = rightleft; i += 1; }
        while (rc > 0) : (rc -= 1) { buf[i] = updown;    i += 1; }
    }
    buf[i] = 'A'; i+=1;
    return buf[0..i];
}

const total_keypads: usize = 2;
const keys_count = math.maxInt(u8)/2;  // this is madnesss - how to do it cleaner ZIG????
var cache = [_][keys_count][keys_count]?usize { [_][keys_count]?usize { [_]?usize {null} ** keys_count } ** keys_count } ** (total_keypads+1);

fn typeKeys(code: []const u8, keypad: usize) usize {
    const first = keypad == total_keypads;
    var buf: [8]u8 = undefined;
    var len: usize = 0;
    var pkey: u8 = 'A';
    const g: u8 = if (first) 3 else 0; // row index of the gap in keypad
    for (code) |nkey| {
        if (cache[keypad][pkey][nkey]) |cmin| {
            len += cmin;
        } else {
            const ppos = if (first) numKeypadPos(pkey) else dirKeypadPos(pkey);
            const npos = if (first) numKeypadPos(nkey) else dirKeypadPos(nkey);
            const updownfirst: []const bool =
                     if (ppos[1] == 0 and npos[0] == g) &[_]bool{ false }
                else if (ppos[0] == g and npos[1] == 0) &[_]bool{ true  }
                else                                    &[_]bool{ true, false };
            const posdiff = .{ ppos[0] - npos[0], ppos[1] - npos[1] };
            var min: usize = math.maxInt(usize);
            for (updownfirst) |udf| {
                const seq = diffToMoves(posdiff, buf[0..], udf);
                const dirlen: usize = if (keypad == 0) 0 else typeKeys(seq, keypad-1);
                var total = seq.len + dirlen;
                if (!first) total -= 1; // ignore trailing 'A' in first keypad
                if (total < min) min = total;
            }
            cache[keypad][pkey][nkey] = min;
            len += min;
        }
        pkey = nkey;
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
        const len = typeKeys(line, total_keypads);
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

test { try std.testing.expectEqual("<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A" .len, typeKeys("029A", total_keypads)); }
test { try std.testing.expectEqual("<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A"         .len, typeKeys("980A", total_keypads)); }
test { try std.testing.expectEqual("<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" .len, typeKeys("179A", total_keypads)); }
test { try std.testing.expectEqual("<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A"     .len, typeKeys("456A", total_keypads)); }
test { try std.testing.expectEqual("<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"     .len, typeKeys("379A", total_keypads)); }

test { try test_solve(126384, "./21/example1.txt"); }
test { try test_solve(94284, "./21/input.txt");    }
// too high: 98508
