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
    sides: usize,
};
var pos_stack: [1024*16*2][2]usize = undefined;
var posi: usize = 0;
var visited_buf: [1024*16*2][2]usize = undefined;
var row_sides_buf: [1024][3]isize = undefined;
var col_sides_buf: [1024][3]isize = undefined;
var rowsi: usize = 0;
var colsi: usize = 0;
fn measureRegion(map: [][]u8, start_ri: usize, start_ci: usize) Region {
    var area: usize = 0;
    var visi: usize = 0;
    posi = 0;
    rowsi = 0;
    colsi = 0;
    const plant = map[start_ri][start_ci];
    pos_stack[posi] = .{start_ri, start_ci}; posi+=1;
    while (posi > 0) {
        posi -= 1;
        const ri = pos_stack[posi][0];
        const ci = pos_stack[posi][1];
        const p = map[ri][ci];
        if (visited(visi, ri,ci)) continue;
        if (p != plant) {
            continue;
        }
        area += 1;
        map[ri][ci] = 0;
        visited_buf[visi] = .{ri,ci}; visi+=1;

        if (ri > 0) {
            pos_stack[posi] = .{ri-1, ci  }; posi+=1;
            if (!visited(visi, ri-1,ci) and map[ri-1][ci]!=plant) {
                pushRowSide(ri, -1, ci);
            }
        }
        else {
            pushRowSide(ri, -1, ci);
        }

        if (ri < map.len-1) {
            pos_stack[posi] = .{ri+1, ci  }; posi+=1;
            if (!visited(visi, ri+1, ci) and map[ri+1][ ci]!=plant) {
                pushRowSide(ri, 1, ci);
            }
        }
        else {
            pushRowSide(ri, 1, ci);
        }

        if (ci > 0) {
            pos_stack[posi] = .{ri,   ci-1}; posi+=1;
            if (!visited(visi, ri,ci-1) and map[ri][ci-1]!=plant) {
                pushColSide(ci, -1, ri);
            }
        }
        else {
            pushColSide(ci, -1, ri);
        }

        if (ci < map[0].len-1) {
            pos_stack[posi] = .{ri,   ci+1}; posi+=1;
            if (!visited(visi, ri,ci+1) and map[ri][ci+1]!=plant) {
                pushColSide(ci, 1, ri);
            }
        }
        else {
            pushColSide(ci, 1, ri);
        }
    }
    const row_sides_count = countSides(row_sides_buf[0..rowsi]);
    const col_sides_count = countSides(col_sides_buf[0..colsi]);
    const sides = row_sides_count + col_sides_count ;
    return .{
        .area = area,
        .sides = sides,
    };
}

fn pushRowSide(ri: usize, d: isize, ci: usize) void {
    row_sides_buf[rowsi]=.{
        @as(isize, @intCast(ri)),
        @as(isize, @intCast(ri))+d,
        @as(isize, @intCast(ci)),
    };
    rowsi+=1;
}

fn pushColSide(ci: usize, d: isize, ri: usize) void {
    col_sides_buf[colsi]=.{
        @as(isize, @intCast(ci)),
        @as(isize, @intCast(ci))+d,
        @as(isize, @intCast(ri)),
    };
    colsi+=1;
}


fn countSides(sides: [][3]isize) usize {
    const sortFn = struct {
        fn sortFn(context: void, lhs: [3]isize, rhs: [3]isize) bool {
            _ = context;
            return std.math.order(lhs[0]*1000000+lhs[1]*1000+lhs[2],
                                  rhs[0]*1000000+rhs[1]*1000+rhs[2]) == .lt;
        }
    }.sortFn;
    std.mem.sort([3]isize, sides, {}, sortFn);
    var count: usize = 1;
    var prev = sides[0];
    for (sides[1..]) |side| {
        if (side[0] != prev[0]) {
            count += 1;
            prev = side;
            continue;
        }
        if (side[0] == prev[0]) {
            if (side[1] != prev[1]) {
                count += 1;
                prev = side;
                continue;
            }
            if (side[1] == prev[1]) {
                if (side[2]-1 != prev[2]) {
                    count += 1;
                }
                prev = side;
                continue;
            }
        }
        unreachable;
    }
    return count;
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
    for (map, 0..) |row, ri| {
        for (row, 0..) |c, ci| {
            if (c == 0) continue;
            const region = measureRegion(map, ri, ci);
            total_cost += region.area * region.sides;
        }
    }
    return total_cost;
}


fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example1" { try test_solve(80,   "./12/example1.txt"); }
test "example2" { try test_solve(236,  "./12/example2.txt"); }
test "example3" { try test_solve(368,  "./12/example3.txt"); }
test "example4" { try test_solve(1206, "./12/example4.txt"); }
test "input"    { try test_solve(808796, "./12/input.txt"); }
