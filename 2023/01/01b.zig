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
    var buff: [1024]u8 = undefined;
    var line: []u8 = undefined;
    var input_has_content = true;

    while (input_has_content) {
        // load line
        var i: usize = 0;
        while (true) {
            const c = reader.readByte() catch |err| switch (err) {
                error.EndOfStream => {
                    input_has_content = false;
                    buff[i] = '\n';
                    i += 1;
                    break;
                },
                else => |e| return e,
            };
            buff[i] = c;
            i += 1;
            if (c == '\n') break;
        }

        line = buff[0..i];
        while (true) : (line.ptr += 1) {
            var c = line[0];
            if (c == '\n') {
                sum += toDigit(last_digit_char);
                //std.debug.print("last '{c}': {d}, sum: {d}\n", .{last_digit_char, toDigit(c)*10, sum});
                last_digit_char = 0;
                break;
            }

            if (!std.ascii.isDigit(c)) {
                     if (std.mem.startsWith(u8, line, "zero"))  { c = '0'; }
                else if (std.mem.startsWith(u8, line, "one"))   { c = '1'; }
                else if (std.mem.startsWith(u8, line, "two"))   { c = '2'; }
                else if (std.mem.startsWith(u8, line, "three")) { c = '3'; }
                else if (std.mem.startsWith(u8, line, "four"))  { c = '4'; }
                else if (std.mem.startsWith(u8, line, "five"))  { c = '5'; }
                else if (std.mem.startsWith(u8, line, "six"))   { c = '6'; }
                else if (std.mem.startsWith(u8, line, "seven")) { c = '7'; }
                else if (std.mem.startsWith(u8, line, "eight")) { c = '8'; }
                else if (std.mem.startsWith(u8, line, "nine"))  { c = '9'; }
                else continue;
            }

            if (last_digit_char == 0) {
                sum += toDigit(c)*10;
                //std.debug.print("first '{c}': {d}, sum: {d}\n", .{c, toDigit(c)*10, sum});
            }
            last_digit_char = c;
        }
    }

    return sum;
}


fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "01b example.a.txt" { try test_solve(142,   "./01/example.a.txt"); }
test "01b example.b.txt" { try test_solve(281,   "./01/example.b.txt"); }
test "01b input.txt"     { try test_solve(56324, "./01/input.txt"); }
