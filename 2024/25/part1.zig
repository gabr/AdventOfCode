const std = @import("std");
const math = std.math;
const mem = std.mem; const Allocator = std.mem.Allocator; const assert = std.debug.assert;
const isWhitespace = std.ascii.isWhitespace;
const dprint = std.debug.print;
//fn dprint(comptime fmt: []const u8, args: anytype) void { _=fmt; _=args; }

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

// singleton global general purpose allocator
fn gpa() Allocator {
    const GPA = std.heap.GeneralPurposeAllocator(.{});
    const S = struct { var gpa: ?GPA = null; };
    if (S.gpa == null) { S.gpa = GPA{}; }
    return S.gpa.?.allocator();
}

fn parseInt(comptime T: type, str: []const u8) !T {
    return try std.fmt.parseInt(T, str, 10);
}

fn solve(reader: anytype) !usize {
    const input = try reader.readAllAlloc(gpa(), std.math.maxInt(usize));
    var lines_it = mem.splitScalar(u8, input, '\n');
    var locks = try std.ArrayList([5]u8).initCapacity(gpa(), 1024);
    var keys  = try std.ArrayList([5]u8).initCapacity(gpa(), 1024);
    while(lines_it.next()) |line_to_trim| {
        const line = std.mem.trim(u8, line_to_trim, "\r \t");
        if (line.len == 0) continue;
        assert(line.len == 5);
        const islock = line[0] == '#' and line[4] == '#';
        var schema = [_]u8 {0}**5;
        for (0..5) |_| {
            const pins_line = std.mem.trim(u8, lines_it.next().?, "\r \t");
            assert(pins_line.len == 5);
            for (pins_line, 0..) |pin, si| {
                if (pin == '#') schema[si] += 1;
            }
        }
        _ = lines_it.next(); // last line
        if (islock) { try locks.append(schema); }
        else        { try keys .append(schema); }
    }
    dprint("debug\n", .{});
    dprint("locks: {d}\n", .{locks.items.len});
    dprint("keys:  {d}\n", .{keys .items.len});
    var sum: usize = 0;
    for (locks.items) |lock| {
        for (keys.items) |key| {
            for (0..5) |si| {
                if (lock[si] + key[si] > 5) break;
            } else {
                sum += 1;
            }
        }
    }
    return sum;
}

fn test_solve(expected: usize, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}

test { try test_solve(3,    "./25/example1.txt"); }
test { try test_solve(2978, "./25/input.txt"); }
