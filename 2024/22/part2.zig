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

fn nPrices(init: usize, n: usize) ![]i8 {
    var prices = try std.ArrayList(i8).initCapacity(gpa(), n+1);
    var secret = init;
    try prices.append(@as(i8, @intCast(secret % 10)));
    for (0..n) |_| {
        secret = nextSecret(secret);
        try prices.append(@as(i8, @intCast(secret % 10)));
    }
    return prices.items;
}

fn diffsToIndex(diffs: [4]i8) usize {
    const b = 19;
    return @as(usize, @intCast(
       (@as(usize, @intCast(diffs[3] + 9)) * b*b*b) +
       (@as(usize, @intCast(diffs[2] + 9)) * b*b  ) +
       (@as(usize, @intCast(diffs[1] + 9)) * b    ) +
        @as(usize, @intCast(diffs[0] + 9))
    ));
}

fn mapPricesToDiffs(prices: []i8) ![]?i8 {
    const b = 19;
    var map = try gpa().alloc(?i8, (b*b*b*b)+2);
    for (0..map.len) |i| { map[i] = null; }
    var diffs: [4]i8 = .{ -9, -9, -9, -9 };
    for (4..prices.len) |i| {
        diffs[0] = prices[i-3] - prices[i-4];
        diffs[1] = prices[i-2] - prices[i-3];
        diffs[2] = prices[i-1] - prices[i-2];
        diffs[3] = prices[i  ] - prices[i-1];
        const di = diffsToIndex(diffs);
        if (map[di] == null) {
            map[di] = prices[i];
        }
    }
    return map;
}

fn buyBananas(diffs: [4]i8, prices_maps: [][]?i8) usize {
    const i = diffsToIndex(diffs);
    var bananas: usize = 0;
    for (prices_maps) |map| {
        if (map[i]) |b| {
            bananas += @intCast(b);
        }
    }
    return bananas;
}

const DiffIt = struct {
    diffs_op: ?[4]i8 = null,

    pub fn next(self: *DiffIt) ?[4]i8 {
        var diffs = self.diffs_op orelse {
            const init_diffs = .{ -9, -9, -9, -9 };
            self.diffs_op = init_diffs;
            return init_diffs;
        };
        var di: usize = 0;
        while (true) {
            if (di == diffs.len) {
                self.diffs_op = null;
                return null;
            }
            if (diffs[di] == 9) {
                diffs[di] = -9;
                di += 1;
                continue;
            }
            diffs[di] += 1;
            self.diffs_op = diffs;
            return diffs;
        }
        unreachable;
    }
};

fn solve(reader: anytype) !usize {
    const input = try reader.readAllAlloc(gpa(), std.math.maxInt(usize));
    var lines_it = mem.splitScalar(u8, input, '\n');
    var buyers = try std.ArrayList([]i8).initCapacity(gpa(), 1024*4);
    while(lines_it.next()) |line_to_trim| {
        const line = std.mem.trim(u8, line_to_trim, "\r \t");
        if (line.len == 0) break;
        const secret = try parseInt(usize, line);
        const prices = try nPrices(secret, 2000);
        try buyers.append(prices);
    }
    var prices_maps = try std.ArrayList([]?i8).initCapacity(gpa(), buyers.items.len);
    for (buyers.items) |prices| {
        try prices_maps.append(try mapPricesToDiffs(prices));
    }
    var max: usize = 0;
    var dit = DiffIt{};
    while (dit.next()) |diffs| {
        const b = buyBananas(diffs, prices_maps.items);
        if (b > max) max = b;
    }
    return max;
}

fn test_solve(expected: usize, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}

test { try test_solve(23,   "./22/example2.txt"); }
test { try test_solve(2272, "./22/input.txt"); }
// too low: 2146
