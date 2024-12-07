const std = @import("std");
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

const MulIterator = struct {
    buf: []const u8,
    i: usize = 0,

    const pattern = "mul(";

    pub fn next(self: *MulIterator) ?u128 {
        while (std.mem.indexOfPosLinear(u8, self.buf, self.i, pattern)) |mi| {
            self.i = mi+pattern.len;
            if (!std.ascii.isDigit(self.buf[self.i])) { self.i+=1; continue; }
            self.i+=1;
            while(self.i < self.buf.len and std.ascii.isDigit(self.buf[self.i])) { self.i+=1; }
            if (self.buf[self.i]!=',') { self.i+=1; continue; }
            const arg1str = self.buf[mi+pattern.len..self.i];
            const arg1 = std.fmt.parseInt(u128, arg1str, 10) catch unreachable;
            self.i+=1;
            if (self.i < self.buf.len and !std.ascii.isDigit(self.buf[self.i])) { self.i+=1; continue; }
            while(self.i < self.buf.len and self.i < self.buf.len and std.ascii.isDigit(self.buf[self.i])) { self.i+=1; }
            if (self.i < self.buf.len and self.buf[self.i]!=')') { self.i+=1; continue; }
            const arg2str = self.buf[mi+pattern.len+arg1str.len+1..self.i];
            const arg2 = std.fmt.parseInt(u128, arg2str, 10) catch unreachable;
            self.i+=1;
            //dprint("MulIterator.next(): {s} -> {d}*{d}\n", .{self.buf[mi..self.i], arg1, arg2});
            return arg1*arg2;
        }
        return null;
    }
};

fn solve(reader: anytype) !u128 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8 ).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    const line = input_al.items;
    var res: u128 = 0;
    var dont_it = std.mem.splitSequence(u8, line, "don't()");
    const first_do = dont_it.next().?;
    //dprint("do: '{s}'\n", .{first_do});
    var mul_it = MulIterator{ .buf = first_do };
    while (mul_it.next()) |mul| { res += mul; }
    while (dont_it.next()) |dont| {
        if (dont.len==0) continue;
        const doi = std.mem.indexOfPosLinear(u8, dont, 0, "do()") orelse continue;
        const do = dont[doi+"do()".len..];
        //dprint("do: '{s}'\n", .{do});
        mul_it = MulIterator{ .buf = do };
        while (mul_it.next()) |mul| { res += mul; }
    }
    return res;
}

fn test_solve(expected: u128, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example" { try test_solve(48,       "./03/example2.txt"); }
test "input"   { try test_solve(75920122, "./03/input.txt"); }

