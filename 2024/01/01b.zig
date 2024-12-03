const std = @import("std");
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

fn solve(reader: anytype) !u64 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8 ).initCapacity(allocator, 1024*16);
    var left_al  = try std.ArrayList(u32).initCapacity(allocator, 1024);
    var right_al = try std.ArrayList(u32).initCapacity(allocator, 1024);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    var line_it = std.mem.splitAny(u8, input_al.items, "\n\r");
    while(line_it.next()) |line| {
      if (line.len == 0) continue;
      var col_it = std.mem.splitSequence(u8, line, "   ");
      const left  = try std.fmt.parseInt(u32, col_it.next().?, 10);
      const right = try std.fmt.parseInt(u32, col_it.next().?, 10);
      try left_al .append(left);
      try right_al.append(right);
    }
    const sortFn = struct {
        fn sortFn(context: void, lhs: u32, rhs: u32) bool {
            _ = context;
            return std.math.order(lhs, rhs) == .lt;
        }
    }.sortFn;
    std.mem.sort(u32, left_al .items, {}, sortFn);
    std.mem.sort(u32, right_al.items, {}, sortFn);
    var similarity: u64 = 0;
    const compareFn = struct {
        fn compareFn(context: void, key: u32, mid_item: u32) std.math.Order {
            _ = context;
            return std.math.order(key, mid_item);
        }
    }.compareFn;
    for (left_al.items) |l| {
        const mi = std.sort.binarySearch(u32, l, right_al.items, {}, compareFn) orelse continue;
        var li = mi; while (li > 0 and right_al.items[li-1] == l) { li-=1; }
        var ri = mi; while (ri+1 < right_al.items.len and right_al.items[ri+1] == l) { ri+=1; }
        const count = ri-li+1;
        similarity += l*count;
    }
    return similarity;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "01b example.b.txt" { try test_solve(31,       "./01/example.a.txt"); }
test "01b input.txt"     { try test_solve(25358365, "./01/input.txt"); }
