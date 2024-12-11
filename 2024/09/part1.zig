const std = @import("std");
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

fn solve(reader: anytype) !u64 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    const line = std.mem.trim(u8, input_al.items, &std.ascii.whitespace);
    var linei: usize = 0;
    var id: usize = 0;
    var disc_buf = try allocator.alloc(u64, 1024*1024);
    const empty = std.math.maxInt(u64);
    var disci: usize = 0;
    var disc_size: usize = 0;
    while (linei < line.len) {
        const file = line[linei];
        var digit = file-'0';
        disc_size += digit;
        for (0..digit) |_| {
            disc_buf[disci] = id;
            disci += 1;
        }
        id += 1;
        linei+=1;
        if (linei >= line.len) break;
        const space = line[linei];
        digit = space-'0';
        disc_size += digit;
        for (0..digit) |_| {
            disc_buf[disci] = empty;
            disci += 1;
        }
        linei+=1;
    }
    const disc = disc_buf[0..disci];
    var l: usize = 0;
    var r: usize = disc.len-1;
    while (true) {
        while (disc[l] != empty) { l += 1; }
        while (disc[r] == empty) { r -= 1; }
        if (l>=r) break;
        disc[l] = disc[r];
        disc[r] = empty;
    }
    var sum: u64 = 0;
    for (disc,0..) |d,i| {
        if (d == empty) break;
        sum+=i*d;
    }
    return sum;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example" { try test_solve(1928, "./09/example1.txt"); }
test "input"   { try test_solve(6432869891895, "./09/input.txt"); }
