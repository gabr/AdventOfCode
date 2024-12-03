const std = @import("std");
const dprint = std.debug.print;
const c = @cImport({
    @cInclude("regex.h");
});

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
    var errcode: c_int = 0;
    var preg: c.regex_t = undefined;
    var pmatch: [4]c.regmatch_t = undefined;
    errcode = c.regcomp(&preg, "(don't\\(\\)|do\\(\\)|mul\\(([0-9]+),([0-9]+)\\))", c.REG_EXTENDED);
    if (errcode > 0) {
        var errbuf: [1024:0]u8 = undefined;
        _ = c.regerror(errcode, &preg, &errbuf, errbuf.len);
        dprint("regcomp error ({d}): {s}\n", .{errcode, errbuf});
        return error.RegcompError;
    }
    var res: u128 = 0;
    var enable = true;
    while (c.regexec(&preg, input, pmatch.len, &pmatch, 0) == 0) {
        const k = input[@as(usize, @intCast(pmatch[0].rm_so)) + 2];
        switch(k) {
            '(' => enable = true,  // do()
            'n' => enable = false, // don't()
            'l' => { // mul()
                if (enable) {
                    const l = try std.fmt.parseInt(u64, input[@as(usize, @intCast(pmatch[2].rm_so))..@as(usize, @intCast(pmatch[2].rm_eo))], 10);
                    const r = try std.fmt.parseInt(u64, input[@as(usize, @intCast(pmatch[3].rm_so))..@as(usize, @intCast(pmatch[3].rm_eo))], 10);
                    res += l*r;
                }
            },
            else => {
                const match = input[@as(usize, @intCast(pmatch[0].rm_so))..@as(usize, @intCast(pmatch[0].rm_eo))];
                dprint("Unknown char '{c}' in match: '{s}'\n", .{k, match});
                return error.UnknownChar;
            }
        }
        input = input[@as(usize, @intCast(pmatch[0].rm_eo))..:0];
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

