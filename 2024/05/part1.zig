const std = @import("std");
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

const Order = struct {
    left: u8,
    right: u8
};

fn isUpdateCorrec(update: []u8, orders: []Order) !bool {
    for (update[1..], 1..) |num, i| {
        for (orders) |order| {
            if (order.left == num) {
                for (update[0..i]) |prev_num| {
                    if (prev_num == order.right) {
                        return false;
                    }
                }
            }
        }
    }
    return true;
}

fn solve(reader: anytype) !u64 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    var line_it = std.mem.splitAny(u8, input_al.items, "\n");
    var orders_al = try std.ArrayList(Order).initCapacity(allocator, 1024);
    while(line_it.next()) |line_to_trim| {
        const line = std.mem.trim(u8, line_to_trim, "\r \t");
        if (line.len == 0) break;
        var split_it = std.mem.splitScalar(u8, line, '|');
        const left_str  = split_it.next().?;
        const right_str = split_it.next().?;
        const order = Order {
            .left  = try std.fmt.parseInt(u8, left_str,  10),
            .right = try std.fmt.parseInt(u8, right_str, 10),
        };
        try orders_al.append(order);
    }
    const orders = orders_al.items;
    var sum: u64 = 0;
    var update_al = try std.ArrayList(u8).initCapacity(allocator, 1024);
    while(line_it.next()) |line_to_trim| {
        const line = std.mem.trim(u8, line_to_trim, "\r \t");
        if (line.len == 0) continue;
        var num_it = std.mem.splitScalar(u8, line, ',');
        update_al.clearRetainingCapacity();
        while (num_it.next()) |num_str| {
            const num = try std.fmt.parseInt(u8, num_str, 10);
            try update_al.append(num);
        }
        const update = update_al.items;
        if (try isUpdateCorrec(update, orders)) {
            if (update.len % 2 == 0) {
                dprint("Update has even number of items: '{s}'\n", .{line});
                return error.updateEvenNumberOfItems;
            }
            sum += update[(update.len-1)/2];
        }
    }
    return sum;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example" { try test_solve(143,  "./05/example1.txt"); }
test "input"   { try test_solve(5208, "./05/input.txt"); }

