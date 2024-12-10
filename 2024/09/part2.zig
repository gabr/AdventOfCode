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
    var input_al = try std.ArrayList(u8).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    const line = std.mem.trim(u8, input_al.items, &std.ascii.whitespace);
    var linei: usize = 0;
    var id: usize = 0;
    var disc_buf = try allocator.alloc(u64, 1024*1024);
    const empty = std.math.maxInt(u64);
    var disci: usize = 0;
    var disc_size: usize = 0;
    var files_al = try std.ArrayList([]u64).initCapacity(allocator, 1024*16);
    while (linei < line.len) {
        const file = line[linei];
        var digit = file-'0';
        disc_size += digit;
        const file_si = disci;
        for (0..digit) |_| {
            disc_buf[disci] = id;
            disci += 1;
        }
        const file_on_disc = disc_buf[file_si..disci];
        try files_al.append(file_on_disc);
        id += 1;
        linei+=1;
        if (linei >= line.len) break;
        const space = line[linei];
        digit = space-'0';
        disc_size += digit;
        for (0..digit) |_| {
            disc_buf[disci] = empty;
            disci += 1;
        }
        linei+=1;
    }
    const disc = disc_buf[0..disci];
    var es: usize = 0;
    var ee: usize = 0;
    //dprint("disc:\n", .{});
    //for (disc) |d| {
    //    if (d == empty) {
    //        dprint(".", .{});
    //    } else {
    //        dprint("{d}", .{d});
    //    }
    //}
    for (0..files_al.items.len-1) |fi| {
        const file = files_al.items[files_al.items.len-1-fi];
        // find empty space for the file
        var empty_space_op: ?[]u64 = null;
        es = 0;
        ee = 0;
        while (true) {
            es = ee+1; while (disc[es]!=empty) { es+=1; if (es==disc.len-1) break;}
            ee = es+1;
            if (ee>=disc.len-1) break;
            while (disc[ee]==empty) { ee+=1; if (ee>=disc.len-1) break;}
            const len = ee-es;
            if (len >= file.len) {
                empty_space_op = disc[es..ee];
                break;
            }
        }
        if (empty_space_op) |empty_space| {
            if (@as(usize, @intFromPtr(empty_space.ptr)) < @as(usize, @intFromPtr(file.ptr))) {
                std.mem.copyForwards(u64, empty_space, file);
                for (file) |*f| { f.* = empty; }
            }
        }
        //dprint("\n", .{});
        //for (disc) |d| {
        //    if (d == empty) {
        //        dprint(".", .{});
        //    } else {
        //        dprint("{d}", .{d});
        //    }
        //}
    }
    //dprint("compacted\n", .{});
    //for (disc) |d| {
    //    if (d == empty) {
    //        dprint(".", .{});
    //    } else {
    //        dprint("{d}", .{d});
    //    }
    //}
    var sum: u64 = 0;
    for (disc,0..) |d,i| {
        if (d != empty) {
            sum+=i*d;
        }
    }
    return sum;
}


fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example" { try test_solve(2858, "./09/example1.txt"); }
test "input"   { try test_solve(6467290479134, "./09/input.txt"); }
