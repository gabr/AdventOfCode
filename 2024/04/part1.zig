const std = @import("std");
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

const XMASCounter = struct {
    res: usize = 0,
    i: usize = 0,

    const XMAS = "XMAS";

    pub fn count(self: *XMASCounter, char: u8) void {
        if (char != XMAS[self.i]) {
            self.i = 0;
            if (char == XMAS[self.i]) self.i += 1;
            return;
        }
        self.i += 1;
        if (self.i == XMAS.len) {
            self.i = 0;
            self.res += 1;
        }
    }
};

fn solve(reader: anytype) !u64 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    var line_it = std.mem.splitAny(u8, input_al.items, "\n\r");
    var rows_al = try std.ArrayList([]const u8).initCapacity(allocator, 1014);
    while(line_it.next()) |line| {
        if (line.len == 0) continue;
        try rows_al.append(line);
    }
    const rows = rows_al.items;
    var counter = XMASCounter{};
    for (rows) |row| {
        for (row) |r| { counter.count(r); }
        counter.i = 0;
        for (0..row.len) |i| { counter.count(row[row.len-i-1]); }
        counter.i = 0;
    }
    for (0..rows[0].len) |ci| {
        for (0..rows.len) |ri| {
            counter.count(rows[ri][ci]);
        }
        counter.i = 0;
        for (0..rows.len) |ri| {
            counter.count(rows[rows.len-ri-1][ci]);
        }
        counter.i = 0;
    }
    for (0..rows[0].len) |ci| {
        var x: usize = ci;
        var y: usize = 0;
        while (true) {
            counter.count(rows[y][x]);
            if (x == 0 or y == rows.len-1) break;
            x-=1;
            y+=1;
        }
        counter.i = 0;
        while (true) {
            counter.count(rows[y][x]);
            if (x == rows[0].len-1 or y == 0) break;
            x+=1;
            y-=1;
        }
        counter.i = 0;
        x = ci;
        y = 0;
        while (true) {
            counter.count(rows[y][x]);
            if (x == rows[0].len-1 or y == rows.len-1) break;
            x+=1;
            y+=1;
        }
        counter.i = 0;
        while (true) {
            counter.count(rows[y][x]);
            if (x == 0 or y == 0) break;
            x-=1;
            y-=1;
        }
        counter.i = 0;
        if (ci == 0) continue;
        x = ci;
        y = rows.len-1;
        while (true) {
            counter.count(rows[y][x]);
            if (x == rows[0].len-1 or y == 0) break;
            x+=1;
            y-=1;
        }
        counter.i = 0;
        while (true) {
            counter.count(rows[y][x]);
            if (x == 0 or y == rows.len-1) break;
            x-=1;
            y+=1;
        }
        counter.i = 0;
        x = rows[0].len-1-ci;
        y = rows.len-1;
        while (true) {
            counter.count(rows[y][x]);
            if (x == 0 or y == 0) break;
            x-=1;
            y-=1;
        }
        counter.i = 0;
        while (true) {
            counter.count(rows[y][x]);
            if (x == rows[0].len-1 or y == rows.len-1) break;
            x+=1;
            y+=1;
        }
        counter.i = 0;
    }
    return counter.res;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example" { try test_solve(18,   "./04/example1.txt"); }
test "input"   { try test_solve(2532, "./04/input.txt"); }

