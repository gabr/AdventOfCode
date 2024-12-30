const std = @import("std");
const math = std.math;
const mem = std.mem; const Allocator = std.mem.Allocator; const assert = std.debug.assert;
const isWhitespace = std.ascii.isWhitespace;
//const dprint = std.debug.print;
fn dprint(comptime fmt: []const u8, args: anytype) void { _=fmt; _=args; }
//fn dprint(comptime fmt: []const u8, args: anytype) void {
//    std.io.getStdOut().writer().print(fmt, args) catch unreachable;
//}

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
    buf[i] = 'A'; i+=1;
    return buf[0..i];
}

fn posDiff(p1: [2]i8, p2: [2]i8) [2]i8 {
    return .{ p1[0] - p2[0], p1[1] - p2[1] };
}

const keypads: usize = 25;
var cache_init: bool = true;
var cache: [keypads]std.StringHashMap(usize) = undefined;
fn typeDirKeys(keys: []const u8, directional_keypads_count: usize) usize {
    //var buf: [8]u8 = undefined;
    var buf = gpa().alloc(u8, 255) catch unreachable;
    defer gpa().free(buf);
    var prevkey: u8 = 'A';
    var prevpos = dirKeypadPos('A');
    var len: usize = 0;
    var up: [2]bool = undefined;
    var upi: usize = 0;
    var cachekeybuf: [2]u8 = undefined;
    for (keys) |key| {
        const nextpos = dirKeypadPos(key);
        cachekeybuf[0] = prevkey;
        cachekeybuf[1] = key;
        if (cache[directional_keypads_count].get(cachekeybuf[0..])) |cmin| {
            len += cmin;
        } else {
            const diff = posDiff(prevpos, nextpos);
                 if (prevpos[1] == 0 and nextpos[0] == 0) { upi = 1; up[0] = false; }
            else if (prevpos[0] == 0 and nextpos[1] == 0) { upi = 1; up[0] = true;  }
            else                                          { upi = 2; up[0] = true;  up[1] = false; }
            var min: usize = math.maxInt(usize);
            for (up[0..upi]) |updownfirst| {
                const seqa = diffToMoves(diff, buf[0..], updownfirst);
                var dirlen: usize = 0;
                if (directional_keypads_count > 0) {
                    dirlen = typeDirKeys(seqa, directional_keypads_count-1);
                }
                const total = seqa.len - 1 + dirlen;
                if (total < min) {
                    min = total;
                    for (1..(keypads+1-directional_keypads_count)) |_| { dprint(" ", .{}); }
                    dprint("new min: seqa '{s}' ({d}), dirlen: {d}, total: {d}\n", .{seqa, seqa.len, dirlen, total});
                }
            }

            const cachekey = gpa().dupe(u8, cachekeybuf[0..]) catch unreachable;
            cache[directional_keypads_count].put(cachekey, min) catch unreachable;
            len += min;
        }
        prevpos = nextpos;
        prevkey = key;
    }
    return len;
}

fn typeKeys(code: []const u8, directional_keypads_count: usize) usize {
    if (cache_init) {
        cache_init = false;
        for (cache[0..]) |*ca| { ca.* = std.StringHashMap(usize).init(gpa()); }
    }
    dprint("debugging code: '{s}'\n", .{code});
    //var buf: [255]u8 = undefined;
    var buf = gpa().alloc(u8, 255) catch unreachable;
    defer gpa().free(buf);
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
        else                                          { upi = 2; up[0] = true;  up[1] = false; }
        var min: usize = math.maxInt(usize);
        for (up[0..upi]) |updownfirst| {
            const seqa = diffToMoves(diff, buf[0..], updownfirst);
            dprint ("{c} ({d},{d}) -> {c} ({d}, {d}) '{s}' ({d})\n", .{
                prevkey, prevpos[0], prevpos[1],
                    key, nextpos[0], nextpos[1],
                seqa, seqa.len });
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
    dprint("typeKeys: '{s}': {d}\n", .{code, len});
    return len;
}

fn solve(reader: anytype) !usize {
    const input = try reader.readAllAlloc(gpa(), std.math.maxInt(usize));
    var lines_it = mem.splitScalar(u8, input, '\n');
    var sum: usize = 0;
    while(lines_it.next()) |line_to_trim| {
        const line = std.mem.trim(u8, line_to_trim, "\r \t");
        if (line.len == 0) break;
        const len = typeKeys(line, keypads);
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

test { try test_solve(116821732384052, "./21/input.txt");    }
// too low:   77996342608406    < debug build
// too low:   87084099621588    < release build - something is wrong
//            87541418752276    hmm fixed this by increasing the buf size to 255 but why?
// too high: 213972469397184
//           190718579945838
