const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
//const dprint = std.debug.print;
fn dprint(comptime fmt: []const u8, args: anytype) void { _=fmt; _=args; }

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

const Machine = struct {
    a: [2]i64,
    b: [2]i64,
    prize: [2]i64,

    pub fn fromLines(
        button_a_line: []const u8,
        button_b_line: []const u8,
        prize_line   : []const u8,
    ) !Machine {
        return .{
            .a     = try extractXY(button_a_line, "Button A: "),
            .b     = try extractXY(button_b_line, "Button B: "),
            .prize = try extractXY(prize_line,    "Prize: "),
        };
    }

    fn extractXY(line: []const u8, header: []const u8) ![2]i64 {
        const trimmed = line[header.len..];
        var it = std.mem.splitSequence(u8, trimmed, ", ");
        const x_str = it.next().?;
        const y_str = it.next().?;
        const x = try std.fmt.parseInt(i64, x_str["X=".len..], 10);
        const y = try std.fmt.parseInt(i64, y_str["Y=".len..], 10);
        return .{ x, y };
    }

    /// Gaussian Elimination algorithm with partial pivoting
    ///
    ///   x0*a0  + x1*b0  =  p0
    ///   x0*a1  + x1*b1  =  p1
    ///
    ///       A       X           B
    ///   +-------+ +----+      +----+
    ///   | a0 b0 | | x0 |  --  | p0 |
    ///   | a1 b1 | | x1 |  --  | p1 |
    ///   +-------+ +----+      +----+
    ///               /\
    ///     AX=B       `-----  buttons presses
    ///
    /// Returns X - which is amout of a and b buttons presses.
    pub fn solve(self: Machine) ?[2]u64 {
        var A: [2][3]f128 = .{
            .{ @floatFromInt(self.a[0]), @floatFromInt(self.b[0]), @floatFromInt(self.prize[0]) },
            .{ @floatFromInt(self.a[1]), @floatFromInt(self.b[1]), @floatFromInt(self.prize[1]) },
        };
        var x: [2]f128 = .{ 0, 0 };
        // partial pivot:
        for (0..x.len) |i| {
            var pivot_row = i;
            for (i+1..x.len) |j| {
                if (@abs(A[j][i]) > @abs(A[pivot_row][i])) {
                    pivot_row = j;
                }
            }
            if (pivot_row != i) {
                for (i..x.len+1) |j| {
                    // swap
                    const tmp = A[i][j];
                    A[i][j] = A[pivot_row][j];
                    A[pivot_row][j] = tmp;
                }
            }
            for (i+1..x.len) |j| {
                if (A[i][i]==0) return null; // no solution
                const factor = A[j][i]/A[i][i];
                for (i..x.len+1) |k| {
                    A[j][k] -= factor * A[i][k];
                }
            }
        }
        // back substitute
        var i = x.len-1;
        while (true) {
            var sum: f128 = 0;
            for (i+1..x.len) |j| {
                sum += A[i][j] * x[j];
            }
            if (A[i][i] == 0) return null; // no solution
            x[i] = (A[i][x.len] - sum)/A[i][i];
            if (i == 0) break;
            i -= 1;
        }
        // validation
        dprint("x: [{d:.3}, {d:.3}]\n", .{x[0], x[1]});
        const a: i64 = @intFromFloat(@round(x[0]));
        const b: i64 = @intFromFloat(@round(x[1]));
        if (a < 0 or a > 100 or
            b < 0 or b > 100) {
            dprint("out of bounds ({d}, {d})\n", .{a,b});
            return null;
        }
        const p0 = a*self.a[0]+b*self.b[0];
        const p1 = a*self.a[1]+b*self.b[1];
        if (p0!=self.prize[0] or
            p1!=self.prize[1]) {
            dprint("incorrect result ({d}!={d} or {d}!={d}\n", .{
                p0, self.prize[0],
                p1, self.prize[1]});
            return null;
        }
        return .{
            @as(u64, @intCast(a)),
            @as(u64, @intCast(b)),
        };
    }
};

fn solve(reader: anytype) !u64 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    var line_it = std.mem.splitAny(u8, input_al.items, "\n");
    dprint("debug:\n", .{});
    var total: u64 = 0;
    while(line_it.next()) |line| {
        if (line.len == 0) continue;
        const machine = try Machine.fromLines(
            line,
            line_it.next().?,
            line_it.next().?,
        );
        dprint("\n", .{});
        dprint("machine: {any}\n", .{machine});
        const solution_op = machine.solve();
        dprint("solve: {any}\n", .{solution_op});
        if (solution_op) |ab| {
            const tokens = ab[0] * 3 + ab[1];
            dprint("tokens: {d}\n", .{tokens});
            total += @intCast(tokens);
        }
    }
    return total;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example1" { try test_solve(480,   "./13/example1.txt"); }
test "input"    { try test_solve(29877, "./13/input.txt"); }
// too high: 44128, 30985
// too low: 17175
// incorrect: 22903
