const std = @import("std");
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

fn scoreTrailhead(map: [][]u8, start_ri: usize, start_ci: usize) u64 {
    const S = struct {
        var paths_buf: [255][2]usize = undefined;
    };
    var peaks_count: usize = 0;
    var pathsi: usize = 0;
    S.paths_buf[pathsi] = .{ start_ri, start_ci }; pathsi+=1;
    while (pathsi>0) {
        pathsi-=1;
        const ri = S.paths_buf[pathsi][0];
        const ci = S.paths_buf[pathsi][1];
        const c = map[ri][ci];
        const d = c-'0';
        if (d == 9) {
            peaks_count += 1;
        } else {
            if (checkDir(map, d, ri, ci, -1,  0)) { S.paths_buf[pathsi] = .{ri-1, ci  }; pathsi+=1; }
            if (checkDir(map, d, ri, ci,  1,  0)) { S.paths_buf[pathsi] = .{ri+1, ci  }; pathsi+=1; }
            if (checkDir(map, d, ri, ci,  0, -1)) { S.paths_buf[pathsi] = .{ri  , ci-1}; pathsi+=1; }
            if (checkDir(map, d, ri, ci,  0,  1)) { S.paths_buf[pathsi] = .{ri  , ci+1}; pathsi+=1; }
        }
    }
    return peaks_count;
}

fn checkDir(map: [][]u8, d: u8, ri: usize, ci: usize, dri: isize, dci: isize) bool {
    const new_ri: isize = @as(isize, @intCast(ri))+dri;
    const new_ci: isize = @as(isize, @intCast(ci))+dci;
    if (new_ri < 0 or new_ri >= map.len or
        new_ci < 0 or new_ci >= map[0].len) return false;
    const c = map[@as(usize, @intCast(new_ri))][@as(usize, @intCast(new_ci))];
    const nd = c-'0';
    return nd != 0 and nd-1 == d;
}

fn solve(reader: anytype) !u64 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    var line_it = std.mem.splitAny(u8, input_al.items, "\n");
    var lines_al = try std.ArrayList([]u8).initCapacity(allocator, 1014);
    while(line_it.next()) |line| {
        if (line.len == 0) continue;
        try lines_al.append(@constCast(line));
    }
    const map = lines_al.items;
    var sum: u64 = 0;
    for (0..map.len) |ri| {
        for (0..map[0].len) |ci| {
            const c = map[ri][ci];
            if (c == '0') {
                const score = scoreTrailhead(map, ri, ci);
                //dprint("[{d}, {d}] scores: {d}\n", .{ri,ci,score});
                sum += score;
            }
        }
    }
    return sum;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example" { try test_solve(81,  "./10/example1.txt"); }
//test "input"   { try test_solve(0, "./10/input.txt"); }
