const std = @import("std");
const mem = std.mem;
const fmt = std.fmt;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

fn solve(reader: anytype) !u64 {
    var id: u32 = 0;
    var sum: u64 = 0;
    var buff: [1024]u8 = undefined;

    line: while (try reader.readUntilDelimiterOrEof(&buff, '\n')) |line| {
        id += 1;
        var iterator = mem.tokenizeAny(u8, line, " ");
        _ = iterator.next(); // skip "Game "
        _ = iterator.next(); // skip "id: "
        while (iterator.next()) |count_str| {
            const count = try fmt.parseInt(u32, count_str, 10);
            const color = iterator.next().?[0];
            if (color == 'r' and count > 12 or
                color == 'g' and count > 13 or
                color == 'b' and count > 14)
                continue :line;
        }
        sum += id;
    }

    return sum;
}

// a second approach without using any buffers and with
// number parsing "on the fly"
fn solve2(reader: anytype) !u64 {
    var id: u32 = 0;
    var sum: u64 = 0;

    line: while (reader.readByte() catch null) |_| {
        try reader.skipUntilDelimiterOrEof(' '); // skip "Game "
        try reader.skipUntilDelimiterOrEof(' '); // skip "id: "
        id += 1;

        while (true) {
            var count: u32 = 0;
            var c = try reader.readByte();
            while (c != ' ') : (c = try reader.readByte()) {
                count = (count*10) + (c & 0b1111);
            }
            c = try reader.readByte();
            if (c == 'r' and count > 12 or
                c == 'g' and count > 13 or
                c == 'b' and count > 14) {
                try reader.skipUntilDelimiterOrEof('\n');
                break;
            }
            while (c != ' ') : (c = try reader.readByte()) {
                if (c == '\n') { sum += id; continue :line; }
            }
        }
    }

    return sum;
}


fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
    try file.seekTo(0); // reset file position for second solve test
    try std.testing.expectEqual(expected, try solve2(file.reader()));
}
test "02a example.txt" { try test_solve(8,    "./02/example.txt"); }
test "02a input.txt"   { try test_solve(2776, "./02/input.txt"); }
