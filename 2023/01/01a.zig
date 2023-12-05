const std = @import("std");

fn toDigit(c: u8) u8 {
    return c & 0b1111;
}

// example.a.txt: 142
// input.txt:     55108
pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    var sum: u64 = 0;
    var last_digit_char: u8 = 0;

    while (true) {
        const c = stdin.readByte() catch |err| switch (err) {
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

    try stdout.print("{d}\n", .{sum});
}
