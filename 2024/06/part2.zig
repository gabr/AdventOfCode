const std = @import("std");
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

const directions = "^>v<";
const empty_field = '.';
const obstackle = '#';
const added_obstackle = 'O';


var marked_pos_buf: [1024*16][2]usize = undefined;
fn loops(map: [][]u8, start_ri: usize, start_ci: usize) !bool {
    // staring direction is always up
    var di: usize = 0;
    var mi: usize = 0;
    var ri: isize = @intCast(start_ri);
    var ci: isize = @intCast(start_ci);
    var loop = false;
    while (true) {
        const dir = directions[di];
        const pri = ri;
        const pci = ci;
        switch (dir) {
            '^'  => ri -= 1,
            '>'  => ci += 1,
            'v'  => ri += 1,
            '<'  => ci -= 1,
            else => return error.UnknownDirection,
        }
        if (ri < 0 or ri >= map.len or
            ci < 0 or ci >= map[0].len) break; // out of map - no loop
        // obstackle - go back, turn and try again
        const c = map[@intCast(ri)][@intCast(ci)];
        if (c == obstackle or c == added_obstackle) {
            ri = pri;
            ci = pci;
            di = (di+1)%directions.len;
            continue;
        }
        if (c == dir) {
            loop = true;
            break;
        }
        // mark current position on the map with current direction
        if (c == empty_field) {
            map[@intCast(ri)][@intCast(ci)] = dir;
            marked_pos_buf[mi] = .{ @intCast(ri), @intCast(ci) }; mi += 1;
        }
    }
    // reset the map
    for (marked_pos_buf[0..mi]) |pos| {
        map[pos[0]][pos[1]] = empty_field;
    }
    return loop;
}

var visited_pos_buf: [1024*16][2]usize = undefined;
fn walk(map: [][]u8, start_ri: usize, start_ci: usize) ![][2]usize {
    var vi: usize = 0;
    var di: usize = 0;
    var ri: isize = @intCast(start_ri);
    var ci: isize = @intCast(start_ci);
    const visited = 'X';
    map[@intCast(ri)][@intCast(ci)] = visited;
    // do not save the starting position
    //visited_pos_buf[vi] = .{ @intCast(ri), @intCast(ci) }; vi += 1;
    while (true) {
        const dir = directions[di];
        const pri = ri;
        const pci = ci;
        switch (dir) {
            '^'  => ri -= 1,
            '>'  => ci += 1,
            'v'  => ri += 1,
            '<'  => ci -= 1,
            else => return error.UnknownDirection,
        }
        if (ri < 0 or ri >= map.len or
            ci < 0 or ci >= map[0].len) break; // out of map
        // obstackle - go back, turn and try again
        if (map[@intCast(ri)][@intCast(ci)] == obstackle) {
            ri = pri;
            ci = pci;
            di = (di+1)%directions.len;
            continue;
        }
        if (map[@intCast(ri)][@intCast(ci)] != visited) {
            visited_pos_buf[vi] = .{ @intCast(ri), @intCast(ci) }; vi += 1;
            map[@intCast(ri)][@intCast(ci)] = visited;
        }
    }
    // reset the map
    for (map) |row| {
        for (row) |*c| {
            // if was marked as move clear it
            if (c.* == visited) {
                c.* = empty_field;
            }
        }
    }
    return visited_pos_buf[0..vi];
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
    // find starting position - direction is always up
    var start_ri: usize = undefined;
    var start_ci: usize = undefined;
    start_pos: for (map,0..) |row,ri| {
        for (row,0..) |c,ci| {
            if (c == '^') {
                start_ri = @intCast(ri);
                start_ci = @intCast(ci);
                break :start_pos;
            }
        }
    }
    const visited_pos = try walk(map, start_ri, start_ci);
    var count: usize = 0;
    for (visited_pos) |pos| {
        // add artificial obstackle and test
        const c = map[pos[0]][pos[1]];
        map[pos[0]][pos[1]] = added_obstackle;
        if (try loops(map, start_ri, start_ci)) count += 1;
        // restore original value
        map[pos[0]][pos[1]] = c;
    }
    return count;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example" { try test_solve(6,    "./06/example1.txt"); }
test "input"   { try test_solve(1951, "./06/input.txt"); }

