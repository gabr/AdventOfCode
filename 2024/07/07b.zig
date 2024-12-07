const std = @import("std");
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

const Operator = enum { plus, multiply, concat };
const ops_count = @typeInfo(Operator).Enum.fields.len;
var ops_buf: [255]Operator = undefined;
fn isEquationPossible(test_value: u64, nums: []u64) !bool {
    const ops = ops_buf[0..nums.len-1];
    for (ops) |*op| { op.* = @as(Operator, @enumFromInt(0)); }
    while (true) {
        const val = try executeEquation(ops, nums);
        if (val == test_value) return true;
        if (!advanceOps(ops)) break;
    }
    return false;
}

fn advanceOps(ops: []Operator) bool {
    var opi: usize = 0;
    while (opi < ops.len) {
        var op: usize = @intFromEnum(ops[opi]);
        op += 1;
        if (op < ops_count) {
            ops[opi] = @enumFromInt(op);
            return true;
        }
        op = 0;
        ops[opi] = @enumFromInt(op);
        opi+=1;
    }
    return false;
}

fn concatNums(a: u64, b: u64) !u64 {
    var mul: u64 = 1;
    while (b >= mul) { mul *= 10; }
    return (a*mul)+b;
}

fn executeEquation(ops: []Operator, nums: []u64) !u64 {
    var res: u64 = nums[0];
    for (0..ops.len) |i| {
        switch (ops[i]) {
            .plus     => res = res+nums[i+1],
            .multiply => res = res*nums[i+1],
            .concat   => res = try concatNums(res,nums[i+1]),
        }
    }
    return res;
}

fn solve(reader: anytype) !u64 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    var line_it = std.mem.splitAny(u8, input_al.items, "\n");
    var num_al = try std.ArrayList(u64).initCapacity(allocator, 1014);
    var sum: u64 = 0;
    while(line_it.next()) |line| {
        if (line.len == 0) continue;
        var colon_it = std.mem.splitSequence(u8, line, ": ");
        const test_value = try std.fmt.parseInt(u64, colon_it.next().?, 10);
        var num_it = std.mem.splitScalar(u8, colon_it.next().?, ' ');
        num_al.clearRetainingCapacity();
        while (num_it.next()) |num_str| {
            try num_al.append(try std.fmt.parseInt(u64, num_str, 10));
        }
        if (try isEquationPossible(test_value, num_al.items)) {
            sum += test_value;
        }
    }
    return sum;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "07b example.a.txt" { try test_solve(11387, "./07/example.a.txt"); }
test "07b input.txt"     { try test_solve(150077710195188, "./07/input.txt"); }

