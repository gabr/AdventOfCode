const std = @import("std");
const math = std.math;
const mem = std.mem; const Allocator = std.mem.Allocator; const assert = std.debug.assert;
const isWhitespace = std.ascii.isWhitespace;
const dprint = std.debug.print;
//fn dprint(comptime fmt: []const u8, args: anytype) void { _=fmt; _=args; }

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

// singleton global general purpose allocator
fn gpa() Allocator {
    const GPA = std.heap.GeneralPurposeAllocator(.{});
    const S = struct { var gpa: ?GPA = null; };
    if (S.gpa == null) { S.gpa = GPA{}; }
    return S.gpa.?.allocator();
}

fn parseInt(comptime T: type, str: []const u8) !T {
    return try std.fmt.parseInt(T, str, 10);
}

const Node = struct {
    id: [2]u8,
    nodes: std.ArrayList(*Node),
};

fn idToIndex(id: [2]u8) usize {
    const base = ('z'-'a')+1;
    const c0 = id[0] - 'a'; assert(c0 < base);
    const c1 = id[1] - 'a'; assert(c1 < base);
    return (@as(usize, @intCast(c0)) * base) +
            @as(usize, @intCast(c1));
}

fn getNodeOrCreate(nodes: []?*Node, id: [2]u8) !*Node {
    const i = idToIndex(id);
    if (nodes[i]) |nodeptr| {
        return nodeptr;
    }
    const node = try gpa().create(Node);
    node.* = .{
        .id = id,
        .nodes = try std.ArrayList(*Node).initCapacity(gpa(), 3),
    };
    nodes[i] = node;
    return node;
}

fn sortIds(ids: [][2]u8) void {
    const sortFn = struct {
        fn sortFn(context: void, lhs: [2]u8, rhs: [2]u8) bool {
            _ = context;
            return std.math.order(idToIndex(lhs), idToIndex(rhs)) == .lt;
        }
    }.sortFn;
    std.mem.sort([2]u8, ids, {}, sortFn);
}

fn idsToStr(ids: [][2]u8) ![]u8 {
    sortIds(ids);
    var str = try gpa().alloc(u8, (ids.len*2)+ids.len-1);
    for (0..ids.len-1) |i| { str[(i*3)+2] = ','; }
    for (ids, 0..) |id, i| {
        str[(i*3)  ] = id[0];
        str[(i*3)+1] = id[1];
    }
    return str;
}

fn makeIdsUnique(ids: [][2]u8) ![][2]u8 {
    sortIds(ids);
    var unique = try std.ArrayList([2]u8).initCapacity(gpa(), 1024);
    try unique.append(ids[0]);
    var prev: usize = idToIndex(ids[0]);
    for (ids[1..]) |id| {
        const i = idToIndex(id);
        if (i != prev) {
            prev = i;
            try unique.append(id);
        }
    }
    return unique.items;
}

fn solve(reader: anytype) !usize {
    const input = try reader.readAllAlloc(gpa(), std.math.maxInt(usize));
    var lines_it = mem.splitScalar(u8, input, '\n');
    const maxnodes = idToIndex(.{ 'z', 'z'});
    const nodes = try gpa().alloc(?*Node, maxnodes+2);
    for (nodes) |*node| { node.* = null; }
    var ids = try std.ArrayList([2]u8).initCapacity(gpa(), 1024);
    //dprint("debug\n", .{});
    while(lines_it.next()) |line_to_trim| {
        const line = std.mem.trim(u8, line_to_trim, "\r \t");
        if (line.len == 0) continue;
        assert(line.len == 5);
        const id1: [2]u8 = @constCast(line[0..2]).*;
        const id2: [2]u8 = @constCast(line[3..5]).*;
        try ids.append(id1);
        try ids.append(id2);
        const node1 = try getNodeOrCreate(nodes, id1);
        const node2 = try getNodeOrCreate(nodes, id2);
        try node1.nodes.append(node2);
        try node2.nodes.append(node1);
    }
    const uniqueids = try makeIdsUnique(ids.items);
    dprint("unique ids: {d}\n", .{uniqueids.len});
    dprint("connections: \n", .{});
    for (uniqueids) |id| {
        const node = nodes[idToIndex(id)].?;
        dprint("{d}: {s} ({d}): ", .{idToIndex(id), id, node.nodes.items.len});
        for (node.nodes.items) |n| {
            dprint("{s}, ", .{n.id});
        }
        dprint("\n", .{});
    }
    var sum: usize = 0;
    var groups = try std.ArrayList([]u8).initCapacity(gpa(), 1024);
    var group: [3][2]u8 = undefined;
    for (uniqueids) |id1| {
        if (id1[0] != 't') continue;
        const node1 = nodes[idToIndex(id1)].?;
        for (node1.nodes.items) |node2| {
            for (node2.nodes.items) |node3| {
                for (node3.nodes.items) |node1from3| {
                    if (idToIndex(id1) == idToIndex(node1from3.id)) {
                        const id2 = node2.id;
                        const id3 = node3.id;
                        group[0][0] = id1[0]; group[0][1] = id1[1];
                        group[1][0] = id2[0]; group[1][1] = id2[1];
                        group[2][0] = id3[0]; group[2][1] = id3[1];
                        const groupstr = try idsToStr(&group);
                        for (groups.items) |g| {
                            if (mem.eql(u8, g, groupstr)) break;
                        } else {
                            //dprint("found: {s}\n", .{groupstr});
                            try groups.append(groupstr);
                            sum += 1;
                        }
                    }
                }
            }
        }
    }
    return sum;
}

fn test_solve(expected: usize, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}

test { try test_solve(7, "./23/example1.txt"); }
test { try test_solve(1400, "./23/input.txt"); }
// too low: 270
