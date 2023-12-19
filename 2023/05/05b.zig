const std = @import("std");
const mem = std.mem;
const fmt = std.fmt;
const maxInt = std.math.maxInt;
const Pool = std.Thread.Pool;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

const MapType = u32;

const MapRange = struct {
    dst:     MapType,
    src_min: MapType,
    src_max: MapType,

    fn lessThan(context: void, lhs: MapRange, rhs: MapRange) bool {
        _ = context;
        return lhs.src_min < rhs.src_min;
    }
};

const Map = struct {
    ranges: []MapRange,
    min:    MapType,
    max:    MapType,
};

const ThreadContext = struct {
    from:         MapType,
    to:           MapType,
    maps:         []Map,
    results:      []MapType,
    result_index: usize,

    pub fn run (ctx: *const ThreadContext) void {
        var   min: MapType = maxInt(MapType);
        var   from = ctx.from;
        const to   = ctx.to;
        const maps = ctx.maps;
        while (from <= to) : (from += 1) {
            var value = from;
            for (maps) |map| {
                // skip map if value not in range
                // can't do that as there are holes in some inputs
                //if (value < map.min or value > map.max) continue;
                // binary search for min range
                var l: usize = 0;
                var r: usize = map.ranges.len-1;
                while (l+1 != r) {
                    const m = l+((r-l)/2);
                    if (value < map.ranges[m].src_min) { r = m; }
                    else { l = m; }
                }
                while (value > map.ranges[l].src_max) { l+=1; }
                value = map.ranges[l].dst + (value - map.ranges[l].src_min);
            }
            if (value < min) min = value;
        }
        ctx.results[ctx.result_index] = min;
    }
};

fn solve(reader: anytype) !u64 {
    var buff: [2*1024]u8 = undefined;
    // parse seeds
    var min_results: [100]MapType = undefined;
    var seeds = try std.BoundedArray(MapType, min_results.len).init(0);
    const seeds_line = (try reader.readUntilDelimiterOrEof(&buff, '\n')).?[("seeds: ".len)..];
    var seeds_iterator = mem.tokenizeAny(u8, seeds_line, " ");
    while (seeds_iterator.next()) |seed| try seeds.append( try fmt.parseInt(MapType, seed, 10));
    // skip empty line after seeds
    _ = try reader.readUntilDelimiterOrEof(&buff, '\n');
    // parse maps
    var map_start_index: usize = 0;
    var current_map: Map = undefined;
    var maps_buffer = try std.BoundedArray(Map, 10).init(0);
    var map_ranges_buffer = try std.BoundedArray(MapRange, 1000).init(0);
    while (try reader.readUntilDelimiterOrEof(&buff, '\n')) |line| {
        if (line.len == 0) {
            current_map.ranges = map_ranges_buffer.slice()[map_start_index..];
            try maps_buffer.append(current_map);
            continue;
        }
        // starting a new map
        if (mem.endsWith(u8, line, " map:")) {
            map_start_index = map_ranges_buffer.len;
            current_map = .{
                .ranges = map_ranges_buffer.buffer[0..0],
                .min = maxInt(MapType),
                .max = 0,
            };
            continue;
        }
        // read map values
        var map_iterator = mem.tokenizeAny(u8, line, " ");
        var map_range = .{
            .dst     = try fmt.parseInt(MapType, map_iterator.next().?, 10),
            .src_min = try fmt.parseInt(MapType, map_iterator.next().?, 10),
            .src_max = try fmt.parseInt(MapType, map_iterator.next().?, 10), // len
        };
        map_range.src_max -= 1;
        map_range.src_max += map_range.src_min;
        if (map_range.src_min < current_map.min) current_map.min = map_range.src_min;
        if (map_range.src_max > current_map.max) current_map.max = map_range.src_max;
        try map_ranges_buffer.append(map_range);
    }
    current_map.ranges = map_ranges_buffer.slice()[map_start_index..];
    try maps_buffer.append(current_map);
    // sort map ranges
    const maps = maps_buffer.slice();
    for (maps) |map| mem.sort(MapRange, map.ranges, {}, MapRange.lessThan);
    // find the smallest location value distributing work among threads
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var thread_pool: Pool = .{
        .allocator = allocator,
        .threads = &[_]std.Thread{},
    };
    try thread_pool.init(.{ .allocator = allocator });
    var thread_contexts = try std.BoundedArray(ThreadContext, 100).init(0);
    var seedsSlice = seeds.slice();
    var min_result_index: usize = 0;
    while (seedsSlice.len > 0) {
        try thread_contexts.append(ThreadContext{
            .from         = seedsSlice[0],
            .to           = seedsSlice[0] + seedsSlice[1],
            .maps         = maps,
            .results      = &min_results,
            .result_index = min_result_index,
        });
        try thread_pool.spawn(ThreadContext.run, .{&thread_contexts.buffer[thread_contexts.len-1]});
        seedsSlice = seedsSlice[2..];
        min_result_index += 1;
    }
    thread_pool.deinit();
    var min_location: MapType = maxInt(MapType);
    for (min_results[0..min_result_index]) |min|
        if (min < min_location) { min_location = min; };
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
