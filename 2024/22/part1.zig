const std = @import("std");
const math = std.math;
const mem = std.mem; const Allocator = std.mem.Allocator; const assert = std.debug.assert;
const isWhitespace = std.ascii.isWhitespace;
const dprint = std.debug.print;
//fn dprint(comptime fmt: []const u8, args: anytype) void { _=fmt; _=args; }

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

fn mix(secret: usize, res: usize) usize {
    return secret^res;
}

fn prune(val: usize) usize {
    return @mod(val, 16777216);
}

fn nextSecret(secret: usize) usize {
    // step 1
    const res1   = secret * 64;
    const mix1   = mix(secret, res1);
    const prune1 = prune(mix1);
    // step 2
    const res2   = prune1 / 32;
    const mix2   = mix(prune1, res2);
    const prune2 = prune(mix2);
    // step 3
    const res3   = prune2 * 2048;
    const mix3   = mix(prune2, res3);
    const prune3 = prune(mix3);
    return prune3;
}

fn nSecret(init: usize, n: usize) usize {
    var secret = init;
    for (0..n) |_| {
        secret = nextSecret(secret);
    }
    return secret;
}

fn solve(reader: anytype) !usize {
    const input = try reader.readAllAlloc(gpa(), std.math.maxInt(usize));
    var lines_it = mem.splitScalar(u8, input, '\n');
    var sum: usize = 0;
    while(lines_it.next()) |line_to_trim| {
        const line = std.mem.trim(u8, line_to_trim, "\r \t");
        if (line.len == 0) break;
        const secret = try parseInt(usize, line);
        const res = nSecret(secret, 2000);
        sum += res;
    }
    return sum;
}

fn test_solve(expected: usize, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}

test {
    const secrets = [_]usize {
        123,
        15887950,
        16495136,
        527345,
        704524,
        1553684,
        12683156,
        11100544,
        12249484,
        7753432,
        5908254,
    };
    for (1..secrets.len) |i| {
        const prev      = secrets[i-1];
        const expected  = secrets[i];
        const generated = nextSecret(prev);
        try std.testing.expectEqual(expected, generated);
    }
}

test { try std.testing.expectEqual(8685429,  nSecret(1,    2000)); }
test { try std.testing.expectEqual(4700978,  nSecret(10,   2000)); }
test { try std.testing.expectEqual(15273692, nSecret(100,  2000)); }
test { try std.testing.expectEqual(8667524,  nSecret(2024, 2000)); }

test { try std.testing.expectEqual(
    37327623,
    nSecret(1,    2000) +
    nSecret(10,   2000) +
    nSecret(100,  2000) +
    nSecret(2024, 2000));
}

test { try test_solve(37327623, "./22/example1.txt"); }
test { try test_solve(20401393616, "./22/input.txt"); }
