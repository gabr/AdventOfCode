const std = @import("std");
const math = std.math;
const mem = std.mem; const Allocator = std.mem.Allocator; const assert = std.debug.assert;
const isWhitespace = std.ascii.isWhitespace;
const dprint = std.debug.print;
//fn dprint(comptime fmt: []const u8, args: anytype) void { _=fmt; _=args; }

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{s}\n", .{try solve(.SUM, stdin)});
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
    //in_gates:  std.ArrayList(*Gate),
    //out_gates: std.ArrayList(*Gate),
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
            //.in_gates  = try std.ArrayList(*Gate).initCapacity(gpa(), 16),
            //.out_gates = try std.ArrayList(*Gate).initCapacity(gpa(), 16),
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
                if (line.val == null) return error.ResultNotCalculated;
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

fn countSimilarBits(z: usize, e: usize, bits: u8) u8 {
    var count: u8 = 0;
    var diff = z ^ e;
    const mask: usize = 1;
    for (0..bits) |_| {
        if (diff & mask == 0) count += 1;
        diff >>= 1;
    }
    return count;
}

fn testAndDevice(device: Device, bits: u8) !u8 {
    const max: usize = @as(usize, @intCast(1)) << @as(u6, @intCast(bits));
    var min_bits: u8 = bits;
    for (0..max) |x| {
        for (0..max) |y| {
            const e = x & y;
            const z = device.run(x, y) catch |err| switch (err) {
                error.ResultNotCalculated => return 0,
                else => return err,
            };
            if (e != z) {
                min_bits = countSimilarBits(z, e, bits);
            }
        }
    }
    return min_bits;
}

fn testSumDevice(device: Device, last_maxb: u8, bits: u8) !u8 {
    var minb: u8 = bits;
    var b: u8 = 0;
    const max_val: usize = @as(usize, @intCast(1)) << @as(u6, @intCast(bits)) - 1;
    const x: usize = 17767980090787;
    const y: usize = 19117176970957;
    const tests = [_][3]usize {
        .{ x, y, x + y },
        //.{ 0, 0, 0 },
        //.{ 1, 0, 1 },
        //.{ 0, 1, 1 },
        //.{ 1, 1, 2 },
        //.{ max_val,           0, max_val },
        .{ max_val-1,         1, max_val },
        //.{ 1,         max_val-1, max_val },
        .{ max_val/2, max_val/2, max_val },
    };
    for (tests) |t| {
        const z = device.run(t[0], t[1]) catch |err| switch (err) {
            error.ResultNotCalculated => 0,
            else => return err,
        };
        b = countSimilarBits(z, t[2], bits);
        if (b < minb) minb = b;
        if (minb < last_maxb) return minb;
        if (minb == 0) return minb;
    }
    return minb;
}

const FixFor = enum { AND, SUM };
fn fixDevice(
    device: Device,
    comptime fixfor: FixFor,
    swapped: *std.ArrayList(*Line),
    goodbits: u8,
    bits: u8
) !bool {
    dprint("fixDevice([{s}])\n", .{ try linesIdsToStr(swapped.items, false) });
    const max_swaps = switch (fixfor) {
        .AND => 2*2,
        .SUM => 4*2,
    };
    for (device.gates.items) |gate1| {
        // if already swapped skip
        if (mem.indexOfScalar(*Line, swapped.items, gate1.out) != null) continue;
        for (device.gates.items) |gate2| {
            // if the same outputs don't swap
            if (mem.eql(u8, gate1.out.id, gate2.out.id)) continue;
            // if already swapped skip
            if (mem.indexOfScalar(*Line, swapped.items, gate2.out) != null) continue;
            // swap and test
            try swapped.append(gate1.out);
            try swapped.append(gate2.out);
            var tmp = gate2.out;
            gate2.out = gate1.out;
            gate1.out = tmp;
            const newgoodbits = switch (fixfor) {
                .AND => try testAndDevice(device, bits),
                .SUM => try testSumDevice(device, goodbits, bits),
            };
            if (newgoodbits >= bits) return true;
            if (newgoodbits > goodbits and swapped.items.len < max_swaps) {
                if (try fixDevice(device, fixfor, swapped, newgoodbits, bits)) return true;
            }
            // unswap
            tmp = gate2.out;
            gate2.out = gate1.out;
            gate1.out = tmp;
            _ = swapped.pop();
            _ = swapped.pop();
        }
    }
    return false;
}

fn linesIdsToStr(lines: []*Line, sort: bool) ![]u8 {
    const S = struct { var buf: []u8 = ""; };
    if (lines.len == 0) return S.buf[0..0];
    const sortFn = struct {
        fn sortFn(context: void, lhs: *Line, rhs: *Line) bool {
            _ = context;
            return mem.lessThan(u8, lhs.id, rhs.id);
        }
    }.sortFn;
    if (sort) std.mem.sort(*Line, lines, {}, sortFn);
    const separator = ",";
    const total_len = blk: {
        var sum: usize = separator.len * (lines.len - 1); // separators
        for (lines) |line| sum += line.id.len;
        break :blk sum;
    };
    if (S.buf.len < total_len) {
        S.buf = try gpa().alloc(u8, total_len);
    }
    @memcpy(S.buf[0..lines[0].id.len], lines[0].id);
    var buf_index: usize = lines[0].id.len;
    for (lines[1..]) |line| {
        @memcpy(S.buf[buf_index .. buf_index + separator.len], separator);
        buf_index += separator.len;
        @memcpy(S.buf[buf_index .. buf_index + line.id.len], line.id);
        buf_index += line.id.len;
    }
    return S.buf[0..total_len];
}

fn solve(comptime fixfor: FixFor, reader: anytype) ![]u8 {
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
        //try in1line.out_gates.append(gate);
        //try in2line.out_gates.append(gate);
        //try outline.in_gates.append(gate);
        try device.gates.append(gate);
    }
    // _ = fixfor;
    //dprint("gates count: {d}, lines count: {d}\n", .{device.gates.items.len, device.lines.items.len});
    //dprint("running for x: {d} ({b}), y: {d} ({b})\n", .{x, x, y, y});
    //dprint("e: {d} ({b})\n", .{x+y, x+y});
    //const z = try device.run(x, y, 0);
    //dprint("z: {d} ({b})\n", .{z, z});
    //return "";
    // count the z00 outputs to determine how many bits should work
    var bits: u8 = 0;
    for (device.lines.items) |line| {
        if (line.id[0] == 'z' and
            std.ascii.isDigit(line.id[1]) and
            std.ascii.isDigit(line.id[2])) {
            bits += 1;
        }
    }
    var swapped = try std.ArrayList(*Line).initCapacity(gpa(), 10);
    const fixed = try fixDevice(device, fixfor, &swapped, 0, bits);
    if (!fixed) return error.DeviceNotFixed;
    return try linesIdsToStr(swapped.items, true);
}

fn test_solve(comptime fixfor: FixFor, expected: []const u8, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqualSlices(u8, expected, try solve(fixfor, file.reader()));
}

test { try test_solve(.AND, "z00,z01,z02,z05", "./24/example3.txt"); }
//test { try test_solve(.SUM, "", "./24/input.txt"); }