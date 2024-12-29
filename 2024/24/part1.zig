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

const Line = struct {
    id:  []const u8,
    val: ?u1,
    in_gates:  std.ArrayList(*Gate),
    out_gates: std.ArrayList(*Gate),
};

const Gate = struct {
    type:    Type,
    in:   [2]*Line,
    out:     *Line,

    const Type = enum {
        AND, // NOTE(arek): Can't have it lover case as that's a reserved word
        OR,
        XOR,

        pub fn fromStr(str: []const u8) !Type {
            if (mem.eql(u8, str, "AND")) return .AND;
            if (mem.eql(u8, str, "XOR")) return .XOR;
            if (mem.eql(u8, str, "OR"))  return .OR;
            dprint("Unknown type: '{s}'\n", .{str});
            return error.UnknownType;
        }

        pub fn toStr(self: Type) []const u8 {
            return switch (self) {
                .AND => "AND",
                .XOR => "XOR",
                .OR  => "OR",
            };
        }
    };

    pub fn init(t: Type, in1: *Line, in2: *Line, out: *Line) Gate {
        return .{
            .type = t,
            .in   = .{ in1, in2, },
            .out  = out,
        };
    }

    pub fn execute(self: Gate) bool {
        if (self.in[0].val == null or
            self.in[1].val == null)
          return false;
        switch (self.type) {
            .AND => self.out.val = self.in[0].val.? & self.in[1].val.?,
            .OR  => self.out.val = self.in[0].val.? | self.in[1].val.?,
            .XOR => self.out.val = self.in[0].val.? ^ self.in[1].val.?,
        }
        return true;
    }
};

const Device = struct {
    lines:  std.ArrayList(*Line),
    gates:  std.ArrayList(*Gate),

    pub fn init() !Device {
        return .{
            .lines  = try std.ArrayList(*Line).initCapacity(gpa(), 1024),
            .gates  = try std.ArrayList(*Gate).initCapacity(gpa(), 1024),
        };
    }

    pub fn getOrCreateLine(self: *Device, id: []const u8) !*Line {
        for (self.lines.items) |line| {
            if (mem.eql(u8, line.id, id)) {
                return line;
            }
        }
        const line = try gpa().create(Line);
        line.* = .{
            .id  = id,
            .val = null,
            .in_gates  = try std.ArrayList(*Gate).initCapacity(gpa(), 16),
            .out_gates = try std.ArrayList(*Gate).initCapacity(gpa(), 16),
        };
        try self.lines.append(line);
        return line;
    }

    pub fn reset(self: Device) void {
        for (self.lines.items) |line| {
            line.val = null;
        }
    }

    pub fn run(self: Device, x: usize, y: usize) !usize {
        self.reset();
        // init x and y values
        for (self.lines.items) |line| {
            if (line.id[0] == 'x' or line.id[0] == 'y') {
                const bit = try parseInt(u6, line.id[1..3]);
                const v = if (line.id[0] == 'x') x else y;
                assert(line.val == null);
                line.val = extractBit(v, bit);
            }
        }
        // run - execute gates with known inputs until none is executed
        var any_executed = true;
        while (any_executed) {
            any_executed = false;
            for (self.gates.items) |gate| {
                if (gate.out.val == null and gate.execute()) {
                    any_executed = true;
                    //dprint("executed: {s} ({d}) {s: <3} {s} ({d}) => {s} ({d})\n", .{
                    //    gate.in[0].id, gate.in[0].val.?,
                    //    gate.type.toStr(),
                    //    gate.in[1].id, gate.in[1].val.?,
                    //    gate.out.id, gate.out.val.?,
                    //});
                }
            }
        }
        // extract result value from z
        var z: usize = 0;
        for (self.lines.items) |line| {
            if (line.id[0] == 'z') {
                const bit = try parseInt(u6, line.id[1..3]);
                assert(line.val != null);
                z = setBit(z, bit, line.val.? == 1);
            }
        }
        return z;
    }
};

fn setBit(v: usize, bit: u6, to: bool) usize {
    const shift: usize = @as(usize, @intCast(1)) << bit;
    if (to) { return v |  shift; }
    else    { return v & ~shift; }
}

fn extractBit(v: usize, bit: u6) u1 {
    const shift: usize = @as(usize, @intCast(1)) << bit;
    return if ((v & shift) > 0) 1 else 0;
}


fn solve(reader: anytype) !usize {
    const input = try reader.readAllAlloc(gpa(), std.math.maxInt(usize));
    var lines_it = mem.splitScalar(u8, input, '\n');
    //dprint("debug\n", .{});
    var x: usize = 0;
    var y: usize = 0;
    while(lines_it.next()) |line_to_trim| {
        const line = std.mem.trim(u8, line_to_trim, "\r \t");
        if (line.len == 0) break;
        const bit = try parseInt(u6, line[1..3]);
        const to  = line[5] == '1';
        if (line[0] == 'x') { x = setBit(x, bit, to); }
        else                { y = setBit(y, bit, to); }
    }
    // parse gates
    var device = try Device.init();
    while(lines_it.next()) |line_to_trim| {
        const line = std.mem.trim(u8, line_to_trim, "\r \t");
        if (line.len == 0) continue;
        var split_it = mem.splitScalar(u8, line, ' ');
        const in1 = split_it.next().?;
        const opt = split_it.next().?;
        const in2 = split_it.next().?;
        _         = split_it.next().?; // ignore arrow
        const out = split_it.next().?;
        const in1line = try device.getOrCreateLine(in1);
        const in2line = try device.getOrCreateLine(in2);
        const outline = try device.getOrCreateLine(out);
        const gate = try gpa().create(Gate);
        gate.* = Gate.init(try Gate.Type.fromStr(opt), in1line, in2line, outline);
        try in1line.out_gates.append(gate);
        try in2line.out_gates.append(gate);
        try outline.in_gates.append(gate);
        try device.gates.append(gate);
    }
    //dprint("gates count: {d}, lines count: {d}\n", .{device.gates.items.len, device.lines.items.len});
    //dprint("running for x: {d} ({b}), y: {d} ({b})\n", .{x, x, y, y});
    const z = try device.run(x, y);
    //dprint("z: {d} ({b})\n", .{z, z});
    return z;
}

fn test_solve(expected: usize, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}

test { try test_solve(4,              "./24/example1.txt"); }
test { try test_solve(2024,           "./24/example2.txt"); }
test { try test_solve(36902370467952, "./24/input.txt");    }
