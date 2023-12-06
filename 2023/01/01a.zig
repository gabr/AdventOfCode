const std = @import("std");

fn toDigit(c: u8) u8 {
    return c & 0b1111;
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

fn solve(reader: anytype) !u64 {
    var sum: u64 = 0;
    var last_digit_char: u8 = 0;

    while (true) {
        const c = reader.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };

        if (c == '\n') {
            sum += toDigit(last_digit_char);
            last_digit_char = 0;
            continue;
        }

        if (!std.ascii.isDigit(c)) {
            continue;
        }

        if (last_digit_char == 0) {
            sum += toDigit(c)*10;
        }
        last_digit_char = c;
    }

    return sum;
}


fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "01a example.a.txt" { try test_solve(142,   "./01/example.a.txt"); }
test "01a input.txt"     { try test_solve(55108, "./01/input.txt"); }
