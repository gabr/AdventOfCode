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
    var input_al = try std.ArrayList(u8 ).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    var line_it = std.mem.splitAny(u8, input_al.items, "\n\r");
    var safe_count: usize = 0;
    line_loop: while(line_it.next()) |line| {
        if (line.len == 0) continue;
        var num_it = std.mem.splitScalar(u8, line, ' ');
        var prev_num = try std.fmt.parseInt(i16, num_it.next().?, 10);
        var num      = try std.fmt.parseInt(i16, num_it.next().?, 10);
        var diff = @abs(prev_num - num);
        if (diff < 1 or diff > 3) continue;
        const asc = num >= prev_num;
        while (num_it.next()) |num_str| {
            prev_num = num;
            num = try std.fmt.parseInt(i16, num_str, 10);
            diff = @abs(prev_num - num);
            if (diff < 1 or diff > 3)     continue :line_loop;
            if (asc != (num >= prev_num)) continue :line_loop;
        }
        safe_count += 1;
    }
    return safe_count;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "02a example.b.txt" { try test_solve(2,   "./02/example.a.txt"); }
test "02a input.txt"     { try test_solve(279, "./02/input.txt"); }
