const std = @import("std");
const Allocator = std.mem.Allocator;
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

const StoneBag = struct {
    stone: u64,
    count: usize,
};
fn blink(allocator: Allocator, times: u64, stones: []u64) !usize {
    var stack1 = try std.ArrayList(?StoneBag).initCapacity(allocator, 1024*16);
    var stack2 = try std.ArrayList(?StoneBag).initCapacity(allocator, 1024*16);
    var curr = &stack1;
    var next = &stack2;
    for (0..stones.len) |i| {
        try curr.append(.{
            .stone = stones[stones.len-1-i],
            .count = 1,
        });
    }
    for (0..times) |_| {
        for (curr.items) |bag_op| {
            const bag = bag_op orelse continue;
            if (bag.stone == 0) {
                try next.append(.{ .stone = 1, .count = bag.count, });
                continue;
            }
            const splited_op = split(bag.stone);
            if (splited_op) |splited| {
                try next.append(.{ .stone = splited[1], .count = bag.count, });
                try next.append(.{ .stone = splited[0], .count = bag.count, });
                continue;
            }
            try next.append(.{ .stone = bag.stone*2024, .count = bag.count, });
        }
        // combine stones
        for (0..next.items.len) |i| {
            const bag = next.items[i] orelse continue;
            for (i+1..next.items.len) |j| {
                const nbag = next.items[j] orelse continue;
                if (nbag.stone == bag.stone) {
                    next.items[i].?.count += nbag.count;
                    next.items[j] = null;
                }
            }
        }
        // switch bufs
        const tmp = next;
        next = curr;
        curr = tmp;
        next.clearRetainingCapacity();
    }
    var count: usize = 0;
    for (curr.items) |bag_op| {
        if (bag_op) |bag| {
            count += bag.count;
        }
    }
    return count;
}

fn split(stone: u64) ?[2]u64 {
    var digits: u8 = 0;
    var tmp = stone;
    while (tmp > 0) {
        digits+=1;
        tmp /= 10;
    }
    if (digits%2!=0) {
        return null;
    }
    const mul = std.math.pow(u64,10,digits/2);
    return .{
        stone / mul,
        stone % mul,
    };
}

fn solve(reader: anytype) !usize {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    var stone_it = std.mem.splitScalar(u8, input_al.items, ' ');
    var stones_al = try std.ArrayList(u64).initCapacity(allocator, 10);
    while (stone_it.next()) |stone_str| {
        const trimed = std.mem.trim(u8, stone_str, &std.ascii.whitespace);
        const stone = try std.fmt.parseInt( u64, trimed, 10);
        try stones_al.append(stone);
    }
    return try blink(allocator, 75, stones_al.items);
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example1" { try test_solve(65601038650482,  "./11/example1.txt"); }
test "input"    { try test_solve(240884656550923, "./11/input.txt"); }
