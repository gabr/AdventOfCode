const std = @import("std");

fn toDigit(c: u8) u8 {
    return c & 0b1111;
}

// example.b.txt: 281
// input.txt:     56324
pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    var sum: u64 = 0;
    var last_digit_char: u8 = 0;
    var buff: [1024]u8 = undefined;
    var line: []u8 = undefined;
    var input_has_content = true;

    while (input_has_content) {
        // load line
        var i: usize = 0;
        while (true) {
            const c = stdin.readByte() catch |err| switch (err) {
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
        while (true) {
            var c = line[0];
            if (c == '\n') {
                sum += toDigit(last_digit_char);
                //std.debug.print("last '{c}': {d}, sum: {d}\n", .{last_digit_char, toDigit(c)*10, sum});
                last_digit_char = 0;
                break;
            }

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

            if (std.ascii.isDigit(c)) {
                if (last_digit_char == 0) {
                    sum += toDigit(c)*10;
                    //std.debug.print("first '{c}': {d}, sum: {d}\n", .{c, toDigit(c)*10, sum});
                }
                last_digit_char = c;
            }
            // which one is better?
            //line = line[1..];
            line.ptr += 1;
        }
    }

    try stdout.print("{d}\n", .{sum});
}
