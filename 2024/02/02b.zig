const std = @import("std");
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

fn isSafe(line: []const u8, skipi_opt: ?usize) !bool {
    dprint("isSafe('{s}', {any}) -> ", .{line, skipi_opt});
    var num_it = std.mem.splitScalar(u8, line, ' ');
    if (skipi_opt) |skipi| { if (skipi == 0) { _=num_it.next(); } }
    var prev_num = try std.fmt.parseInt(i16, num_it.next().?, 10);
    if (skipi_opt) |skipi| { if (skipi == 1) { _=num_it.next(); } }
    var num      = try std.fmt.parseInt(i16, num_it.next().?, 10);
    var diff = @abs(prev_num - num);
    if (diff < 1 or diff > 3) {
        dprint("?\n", .{});
        return skipi_opt == null and (try isSafe(line, 0) or try isSafe(line, 1));
    }
    const asc = num >= prev_num;
    var i: usize = 2;
    while (num_it.next()) |num_str| : (i+=1) {
        if (skipi_opt) |skipi| { if (skipi == i) { dprint("(skiped {s}) ", .{num_str}); continue; } }
        prev_num = num;
        num = try std.fmt.parseInt(i16, num_str, 10);
        diff = @abs(prev_num - num);
        if ((diff < 1 or diff > 3) or (asc != (num >= prev_num))) {
            if (skipi_opt == null) {
                dprint("??\n", .{});
                return
                    try isSafe(line, 0) or
                    try isSafe(line, 1) or
                    try isSafe(line, i) or
                    try isSafe(line, i+1);
            } else {
                dprint("false\n", .{});
                return false;
            }
        }
        i+=1;
    }
    dprint("true\n", .{});
    return true;
}

fn solve(reader: anytype) !u64 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8 ).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    var line_it = std.mem.splitAny(u8, input_al.items, "\n\r");
    var safe_count: usize = 0;
    while(line_it.next()) |line| {
        if (line.len == 0) continue;
        if (try isSafe(line, null)) {
            //dprint("safe: {s}\n", .{line});
            safe_count += 1;
        }
    }
    return safe_count;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "02b example.b.txt" { try test_solve(4,   "./02/example.a.txt"); }
test "02b input.txt"     { try test_solve(335, "./02/input.txt"); }

//test { try std.testing.expectEqual(true, try isSafe("12 6 4 2 1", null)); }
//test { try std.testing.expectEqual(true, try isSafe("6 4 2 1 12", null)); }
//test { try std.testing.expectEqual(true, try isSafe("6 90 4 2 1", null)); }
//test { try std.testing.expectEqual(true, try isSafe("1 4 2 1", null)); }

