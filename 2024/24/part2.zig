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

const Line = struct {
    id:  []const u8,
    val: ?u1,
    in_gate:   ?*Gate,
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

    pub fn toStr(self: Gate) ![]u8 {
        const S = struct { var buf: [80]u8 = undefined; };
        return try std.fmt.bufPrint(S.buf[0..], "{s} {s: <3} {s} => {s}\n", .{
            self.in[0].id,
            self.type.toStr(),
            self.in[1].id,
            self.out.id,
        });
    }
};

const Device = struct {
    lines:  std.ArrayList(*Line),
    gates:  std.ArrayList(*Gate),
    stack:  std.ArrayList(*Gate),

    pub fn init() !Device {
        return .{
            .lines = try std.ArrayList(*Line).initCapacity(gpa(), 1024),
            .gates = try std.ArrayList(*Gate).initCapacity(gpa(), 1024),
            .stack = try std.ArrayList(*Gate).initCapacity(gpa(), 1024),
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
            .in_gate   = null,
            .out_gates = try std.ArrayList(*Gate).initCapacity(gpa(), 16),
        };
        try self.lines.append(line);
        return line;
    }

    pub fn reset(self: *Device) void {
        self.stack.clearRetainingCapacity();
        for (self.lines.items) |line| {
            line.val = null;
        }
    }

    fn pushNextGatesToStack(self: *Device) !void {
        for (self.gates.items) |gate| {
            if (gate.out.val == null and
                gate.in[0].val != null and
                gate.in[1].val != null) {
                try self.stack.append(gate);
            }
        }
    }

    pub fn run(self: *Device, x: usize, y: usize) !usize {
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
        // run
        for (0..math.maxInt(usize)) |i| {
            try self.pushNextGatesToStack();
            if (self.stack.items.len == 0) break;
            dprint("round: {d}, gates: {d}\n", .{i, self.stack.items.len});
            while (self.stack.popOrNull()) |gate| {
                assert(gate.out.val == null);
                assert(gate.execute());
                dprint("  executed: {s} ({d}) {s: <3} {s} ({d}) => {s} ({d})\n", .{
                    gate.in[0].id, gate.in[0].val.?,
                    gate.type.toStr(),
                    gate.in[1].id, gate.in[1].val.?,
                    gate.out.id, gate.out.val.?,
                });
            }
        }
        // extract result value from z
        var z: usize = 0;
        for (self.lines.items) |line| {
            if (line.id[0] == 'z') {
                const bit = try parseInt(u6, line.id[1..3]);
                if (line.val == null) return error.ResultNotCalculated;
                z = setBit(z, bit, line.val.? == 1);
            }
        }
        return z;
    }

    fn findZ(self: Device, bit: usize) !?*Line {
        assert(bit >= 0);
        assert(bit <= 99);
        const S = struct { var buf: [4]u8 = undefined; };
        const id = try std.fmt.bufPrint(S.buf[0..], "{d:0>2}", .{bit});
        for (self.lines.items) |line| {
            if (mem.eql(u8, line.id, id)) return line;
        }
        return null;
    }

    pub fn validate(self: *Device) !bool {
        var bit: usize = 0;
        while (try self.findZ(bit)) |line| : (bit += 1) {
            // the gate connected directly to the z00 should be a XOR gate
            const z_xor_gate = line.in_gate orelse {
                dprint("Missing input gate for {s} output\n", .{line.id});
                return error.MissingZinputGate;
            };
            if (z_xor_gate.type != .XOR) {
                dprint("The gate connected to the output '{s}' is not a XOR gate but: {s}\n", .{
                    line.id, try z_xor_gate.toStr() });
            }
        }
        return true;
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

fn solve(reader: anytype) ![]u8 {
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
        if (outline.in_gate != null) {
            dprint("line '{s}' already has an input gate: {s}, but a next one tries to attach: {s}\n", .{
                outline.id, try outline.in_gate.?.toStr(), try gate.toStr() });
            return error.TwoInputGatesFound;
        }
        outline.in_gate = gate;
        try device.gates.append(gate);
    }
    dprint("gates count: {d}, lines count: {d}\n", .{device.gates.items.len, device.lines.items.len});
    dprint("running for x: {d} ({b}), y: {d} ({b})\n", .{x, x, y, y});
    const e = x+y;
    const z = try device.run(x, y);
    dprint("e: {d: >20} ({b:_>50})\n", .{e,e});
    dprint("z: {d: >20} ({b:_>50})\n", .{z, z});
    dprint("d: {d: >20} ({b:_>50})\n", .{z^e, z^e});
    //_ = try device.validate();
    return "";
}

// solution found manually using this program as a "debugger"
// cvp,mkk,qbw,wcb,wjb,z10,z14,z34
