const std = @import("std");
const math = std.math;
const mem = std.mem; const Allocator = std.mem.Allocator; const assert = std.debug.assert;
const isWhitespace = std.ascii.isWhitespace;
const dprint = std.debug.print;
//fn dprint(comptime fmt: []const u8, args: anytype) void { _=fmt; _=args; }

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{s}\n", .{try solve(stdin)});
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

const maxnodes = idToIndex(.{ 'z', 'z'});
const Node = struct {
    id: [2]u8,
    nodes: std.ArrayList(*Node),
    group: std.ArrayList(usize),

    fn inGroup(self: Node, idx: usize) bool {
        for (self.group.items) |gidx| {
            if (gidx == idx) return true;
        }
        return false;
    }
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
        .nodes = try std.ArrayList(*Node).initCapacity(gpa(), 16),
        .group = try std.ArrayList(usize).initCapacity(gpa(), 16),
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

fn solve(reader: anytype) ![]u8 {
    const input = try reader.readAllAlloc(gpa(), std.math.maxInt(usize));
    var lines_it = mem.splitScalar(u8, input, '\n');
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
    //dprint("unique ids: {d}\n", .{uniqueids.len});
    //dprint("connections: \n", .{});
    //for (uniqueids) |id| {
    //    const node = nodes[idToIndex(id)].?;
    //    dprint("{d}: {s} ({d}): ", .{idToIndex(id), id, node.nodes.items.len});
    //    for (node.nodes.items) |n| {
    //        dprint("{s}, ", .{n.id});
    //    }
    //    dprint("\n", .{});
    //}
    var maxsize: usize = 0;
    var maxgroup: []usize = undefined;
    var group = try std.ArrayList(usize).initCapacity(gpa(), 1024);
    for (uniqueids) |id1| {
        group.clearRetainingCapacity();
        const idx1 = idToIndex(id1);
        try group.append(idx1);
        for (uniqueids) |id2| {
            const idx2 = idToIndex(id2);
            if (idx1 == idx2) continue;
            const node2 = nodes[idx2].?;
            const connects_to_all_in_group =
                for (group.items) |gidx| {
                    const in_group = for (node2.nodes.items) |n2n| {
                        const n2nidx = idToIndex(n2n.id);
                        if (n2nidx == gidx) break true;
                    } else false;
                    if (!in_group) break false;
                } else true;
            if (connects_to_all_in_group) try group.append(idx2);
        }
        if (group.items.len > maxsize) {
            maxsize = group.items.len;
            maxgroup = try gpa().dupe(usize, group.items);
        }
    }
    if (maxsize == 0) return error.NotFoundAny;
    var maxgroupids = try std.ArrayList([2]u8).initCapacity(gpa(), 1024);
    for (maxgroup) |idx| {
        const id = nodes[idx].?.id;
        try maxgroupids.append(.{ id[0], id[1] });
    }
    sortIds(maxgroupids.items);
    return idsToStr(maxgroupids.items);
}

fn test_solve(expected: []const u8, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqualSlices(u8, expected, try solve(file.reader()));
}

test { try test_solve("co,de,ka,ta", "./23/example1.txt"); }
//test { try test_solve("", "./23/input.txt"); }
