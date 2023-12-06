const std = @import("std");
const mem = std.mem;
const fmt = std.fmt;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

fn solve(reader: anytype) !u64 {
    var sum: u64 = 0;
    var buff: [1024]u8 = undefined;

    var rmax: u16 = undefined;
    var gmax: u16 = undefined;
    var bmax: u16 = undefined;

    while (try reader.readUntilDelimiterOrEof(&buff, '\n')) |line| {
        rmax = 0;
        gmax = 0;
        bmax = 0;
        var iterator = mem.tokenizeAny(u8, line, " ");
        _ = iterator.next(); // skip "Game "
        _ = iterator.next(); // skip "id: "
        while (iterator.next()) |count_str| {
            const count = try fmt.parseInt(u16, count_str, 10);
            const color = iterator.next().?[0];
                 if (color == 'r') { if (count > rmax) rmax = count; }
            else if (color == 'g') { if (count > gmax) gmax = count; }
            else if (color == 'b') { if (count > bmax) bmax = count; }
            else return error.UnknownColor;
        }
        sum += (rmax * gmax * bmax);
    }

    return sum;
}


fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "02b example.txt" { try test_solve(2286,  "./02/example.txt"); }
test "02b input.txt"   { try test_solve(68638, "./02/input.txt"); }
