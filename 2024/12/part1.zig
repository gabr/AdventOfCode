const std = @import("std");
const Allocator = std.mem.Allocator;
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

const Region = struct {
    area: usize,
    perimeter: usize,
};
var pos_stack: [1024*16*2][2]usize = undefined;
var visited_buf: [1024*16*2][2]usize = undefined;
fn measureRegion(map: [][]u8, start_ri: usize, start_ci: usize) Region {
    var area: usize = 0;
    var perimeter: usize = 0;
    var posi: usize = 0;
    var visi: usize = 0;
    const plant = map[start_ri][start_ci];
    pos_stack[posi] = .{start_ri, start_ci}; posi+=1;
    while (posi > 0) {
        posi -= 1;
        const ri = pos_stack[posi][0];
        const ci = pos_stack[posi][1];
        const p = map[ri][ci];
        if (visited(visi, ri,ci)) continue;
        if (p != plant) {
            perimeter += 1;
            continue;
        }
        area += 1;
        map[ri][ci] = 0;
        visited_buf[visi] = .{ri,ci}; visi+=1;
        if (ri > 0)            { pos_stack[posi] = .{ri-1, ci  }; posi+=1; } else { perimeter += 1; }
        if (ri < map.len-1)    { pos_stack[posi] = .{ri+1, ci  }; posi+=1; } else { perimeter += 1; }
        if (ci > 0)            { pos_stack[posi] = .{ri,   ci-1}; posi+=1; } else { perimeter += 1; }
        if (ci < map[0].len-1) { pos_stack[posi] = .{ri,   ci+1}; posi+=1; } else { perimeter += 1; }
    }
    //dprint("plant '{c}', area: {d}, perimeter: {d}\n", .{plant, area, perimeter});
    return .{
        .area = area,
        .perimeter = perimeter,
    };
}

fn visited(visi: usize, ri: usize, ci: usize) bool {
    for (0..visi) |i| {
        const vis = visited_buf[i];
        if (ri == vis[0] and ci == vis[1]) return true;
    }
    return false;
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
    var total_cost: usize = 0;
    //dprint("debug:\n", .{});
    for (map, 0..) |row, ri| {
        for (row, 0..) |c, ci| {
            if (c == 0) continue;
            const region = measureRegion(map, ri, ci);
            total_cost += region.area * region.perimeter;
        }
    }
    return total_cost;
}


fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example1" { try test_solve(140,  "./12/example1.txt"); }
test "example2" { try test_solve(772,  "./12/example2.txt"); }
test "example3" { try test_solve(1930, "./12/example3.txt"); }
test "input"    { try test_solve(1352976, "./12/input.txt"); }
