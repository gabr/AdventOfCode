const std = @import("std");
const mem = std.mem;
const fmt = std.fmt;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

fn solve(reader: anytype) !u64 {
    var line_buff: [1024]u8 = undefined;
    var card_index: usize = 0;
    var cards_count = [_]u64{1} ** 1024;
    var winning_numbers = try std.BoundedArray(u8, 64).init(0);
    var haved_numbers   = try std.BoundedArray(u8, 64).init(0);

    while (try reader.readUntilDelimiterOrEof(&line_buff, '\n')) |line| : (card_index += 1) {
        // split line into winning numbers and haved numbers
        const comma_index = mem.indexOfScalar(u8, line, ':').?;
        const pipe_index  = mem.indexOfScalar(u8, line, '|').?;
        const winning_numbers_string = line[(comma_index+2)..pipe_index];
        const haved_numbers_string   = line[(pipe_index+2)..];
        // parse winning numbers into array
        var iterator = mem.tokenizeAny(u8, winning_numbers_string, " ");
        try winning_numbers.resize(0);
        while (iterator.next()) |number_string| try winning_numbers.append(try fmt.parseInt(u8, number_string, 10));
        // parse haved numbers into array
        iterator = mem.tokenizeAny(u8, haved_numbers_string, " ");
        try haved_numbers.resize(0);
        while (iterator.next()) |number_string| try haved_numbers.append(try fmt.parseInt(u8, number_string, 10));
        // count card score
        var card_score: u64 = 0;
        const winning_numbers_slice = winning_numbers.constSlice();
        for (haved_numbers.constSlice()) |number| {
            if (mem.indexOfScalar(u8, winning_numbers_slice, number) != null) {
                card_score += 1;
            }
        }
        for (1..(card_score+1)) |i| cards_count[card_index+i] += cards_count[card_index];
    }

    var sum: u64 = 0;
    for (cards_count[0..card_index]) |card_count| sum += card_count;
    return sum;
}


fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "04b example.txt" { try test_solve(30,    "./04/example.txt"); }
test "04b input.txt"   { try test_solve(8736438, "./04/input.txt"); }

