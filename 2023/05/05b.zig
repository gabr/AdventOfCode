const std = @import("std");
const mem = std.mem;
const fmt = std.fmt;
const maxInt = std.math.maxInt;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

fn solve(reader: anytype) !u64 {
    var buff: [2*1024]u8 = undefined;
    // parse seeds
    var seeds = try std.BoundedArray(u64, 100).init(0);
    const seeds_line = (try reader.readUntilDelimiterOrEof(&buff, '\n')).?[("seeds: ".len)..];
    var seeds_iterator = mem.tokenizeAny(u8, seeds_line, " ");
    while (seeds_iterator.next()) |seed| try seeds.append( try fmt.parseInt(u64, seed, 10));
    // skip empty line after seeds
    _ = try reader.readUntilDelimiterOrEof(&buff, '\n');
    // parse maps
    var maps = try std.BoundedArray(?u64, 10*1024).init(0);
    while (try reader.readUntilDelimiterOrEof(&buff, '\n')) |line| {
        // append null on empty line to separate maps
        if (line.len == 0) {
            try maps.append(null);
            continue;
        }
        // skip map description line header
        if (mem.endsWith(u8, line, " map:")) {
            continue;
        }
        // read map values
        var map_iterator = mem.tokenizeAny(u8, line, " ");
        try maps.append(try fmt.parseInt(u64, map_iterator.next().?, 10)); // destination
        try maps.append(try fmt.parseInt(u64, map_iterator.next().?, 10)); // source
        try maps.append(try fmt.parseInt(u64, map_iterator.next().?, 10)); // len
    }
    // append extra null at the end to simplify conditions later on
    try maps.append(null);
    // find the smallest location value
    var min_location: u64 = maxInt(u64);
    var seedsSlice = seeds.slice();
    while (seedsSlice.len > 0) : (seedsSlice = seedsSlice[2..]) {
        var from = seedsSlice[0];
        const to = from + seedsSlice[1];
        while (from <= to) : (from += 1) {
            var value = from;
            var mapsSlice = maps.constSlice();
            while (mapsSlice.len > 0) {
                if (mapsSlice[0] == null) {
                    mapsSlice = mapsSlice[1..];
                    continue;
                }
                // unpack map
                const dst = mapsSlice[0].?;
                const src = mapsSlice[1].?;
                const len = mapsSlice[2].?;
                mapsSlice = mapsSlice[3..];
                // update value if in range and move to the next map
                if (value >= src and value <= src + len) {
                    value = dst + (value - src);
                    while (mapsSlice[0] != null) mapsSlice = mapsSlice[1..];
                    mapsSlice = mapsSlice[1..];
                }
            }
            if (value < min_location) min_location = value;
        }
    }
    return min_location;
}


fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "05b example.txt" { try test_solve(46,        "./05/example.txt"); }
// it takes between 10 to 15 mins to brute force the second solution so it's commented out
//test "05b input.txt"   { try test_solve(60568880, "./05/input.txt"); }
