const std = @import("std");
const Cregex = @import("cregex.zig").Cregex;
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

fn solve(reader: anytype) !u128 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8 ).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    try input_al.append(0);
    var input = input_al.items[0..input_al.items.len-1:0];
    var cregex = try Cregex(4).init("(don't\\(\\)|do\\(\\)|mul\\(([0-9]+),([0-9]+)\\))");
    var res: u128 = 0;
    var enable = true;
    while (try cregex.match(input)) |matches| {
        const k = matches[0].?.val[2];
        switch(k) {
            '(' => enable = true,  // do()
            'n' => enable = false, // don't()
            'l' => { // mul()
                if (enable) {
                    const l = try std.fmt.parseInt(u64, matches[2].?.val, 10);
                    const r = try std.fmt.parseInt(u64, matches[3].?.val, 10);
                    res += l*r;
                }
            },
            else => {
                dprint("Unknown char '{c}' in match: '{s}'\n", .{k, matches[0].?.val});
                return error.UnknownChar;
            }
        }
        input = input[matches[0].?.pos+matches[0].?.val.len..:0];
    }
    return res;
}

fn test_solve(expected: u128, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "03b example.b.txt" { try test_solve(48,       "./03/example.b.txt"); }
test "03b input.txt"     { try test_solve(75920122, "./03/input.txt"); }

