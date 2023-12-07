const std = @import("std");
const mem = std.mem;
const fmt = std.fmt;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

const Value = struct {
    pos:   usize,
    value: u16,
    len:   u8,
    added: bool,
};

fn solve(reader: anytype) !u64 {
    var sum:         u64   = 0;
    var index:       usize = 0;
    var line_length: usize = 0;
    // TODO: optimize: store offsets istead of absolute indexes
    const size: usize = 1024*2;
    var symbols_pos: [size]usize = undefined;
    var values:      [size]Value = undefined;
    var symbols_count: usize = 0;
    var values_count:  usize = 0;

    while (reader.readByte() catch null) |byte| : (index += 1) {
        var c = byte;

        if (std.ascii.isDigit(c)) {
            var value: u16 = 0;
            var len:    u8 = 0;
            while (std.ascii.isDigit(c)) : (c = try reader.readByte()) {
                value = (value * 10) + (c & 0b1111);
                len += 1;
            }
            values[values_count] = .{
                .pos   = index,
                .value = value,
                .len   = len,
                .added = false
            };
            values_count += 1;
            index += len;
        }

        if (c != '.' and c != '\n') {
            symbols_pos[symbols_count] = index;
            symbols_count += 1;
        }

        if (c == '\n') {
            if (line_length == 0) line_length = index;
            index -= 1;
        }
    }

    for (symbols_pos[0..symbols_count]) |pos| {
        var adjacent     = [_]usize{pos} ** 8; // pos is default as invalid to skip
        const left_edge  = pos % line_length == 0;
        const right_edge = (pos + 1) % line_length == 0;

        if (!left_edge and pos > (line_length + 1))   adjacent[0] = pos - line_length - 1;
        if (pos >  line_length)                       adjacent[1] = pos - line_length;
        if (!right_edge and pos > (line_length - 1))  adjacent[2] = pos - line_length + 1;
        if (!left_edge and pos > 1)                   adjacent[3] = pos - 1;
        if (!right_edge)                              adjacent[4] = pos + 1;
        if (!left_edge)                               adjacent[5] = pos + line_length - 1;
                                                      adjacent[6] = pos + line_length;
        if (!right_edge)                              adjacent[7] = pos + line_length + 1;

        // TODO: optimize: we don't have to check all the values - only certain range
        values_loop: for (values[0..values_count]) |*value| {
            if (value.added) continue;
            for (adjacent) |a| {
                if (a == pos) continue; // skip invalid positions
                var vp = value.pos;
                while (vp < value.pos + value.len) : (vp += 1) {
                    if (a == vp) {
                        sum += value.value;
                        value.added = true;
                        continue :values_loop;
                    }
                }
            }
        }
    }

    return sum;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "03a example.txt" { try test_solve(4361,   "./03/example.txt"); }
test "03a input.txt"   { try test_solve(520019, "./03/input.txt"); }
