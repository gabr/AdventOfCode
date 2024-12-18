const std = @import("std");
const math = std.math;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
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

const CPU = struct {
    a: u64,
    b: u64,
    c: u64,
    ip: usize,
    mem: [] u8,

    pub fn step(self: *CPU) !?u64 {
        if (self.ip == self.mem.len) return error.Halt;
        const opcode = self.mem[self.ip];
        self.ip+=1;
        if (self.ip == self.mem.len) return error.MissingOperand;
        const operand = self.mem[self.ip];
        self.ip+=1;
        //dprint("running opcode {d} with operand {d} (ip: {d})\n", .{opcode, operand, self.ip});
        switch (opcode) {
            0 => {        try self.adv(operand); return null; },
            1 => {        try self.bxl(operand); return null; },
            2 => {        try self.bst(operand); return null; },
            3 => {        try self.jnz(operand); return null; },
            4 => {        try self.bxc();        return null; },
            5 => { return try self.out(operand);              },
            6 => {        try self.bdv(operand); return null; },
            7 => {        try self.cdv(operand); return null; },
            else => {
                dprint("Unknown opcode: {d}\n", .{opcode});
                return error.UnknownOpcode;
            },
        }
    }

    fn comboval(self: CPU, operand: u8) !u64 {
        switch (operand) {
            0 => return 0,
            1 => return 1,
            2 => return 2,
            3 => return 3,
            4 => return self.a,
            5 => return self.b,
            6 => return self.c,
            else => {
                dprint("Unknown operand: {d}\n", .{operand});
                return error.UnknownOperand;
            },
        }
    }

    fn adv(self: *CPU, operand: u8) !void {
        const opval = try self.comboval(operand);
        const denom = math.pow(u64, 2, opval);
        const res = self.a/denom;
        //dprint("adv({d}): {D}/2^{d} -> {d}/{d} = {d}\n", .{operand, self.a, opval, self.a, denom, res});
        self.a = res;
    }

    fn bxl(self: *CPU, operand: u8) !void {
        const opval: u64 = @intCast(operand);
        const res = self.b ^ opval;
        //dprint("bxl({d}): {d} (b{b})^{d} (b{b}) -> {d} (b{b})\n", .{operand, self.b, self.b, opval, opval, res, res});
        self.b = res;
    }

    fn bst(self: *CPU, operand: u8) !void {
        const opval = try self.comboval(operand);
        const res = opval & 0b111;
        //dprint("bst({d}): {d} (b{b}) & 0b111 -> {d} (b{b})\n", .{operand, opval, opval, res, res});
        self.b = res;
    }

    fn jnz(self: *CPU, operand: u8) !void {
        if (self.a == 0) {
            //dprint("jnz({d}): a == 0 - no jump\n", .{operand});
            return;
        }
        const opval: usize = @intCast(operand);
        //dprint("jnz({d}): ip: {d} -> {d}\n", .{operand, self.ip, opval});
        self.ip = opval;
    }

    fn bxc(self: *CPU) !void {
        const res = self.b ^ self.c;
        //dprint("bxc(): {d} (b{b}) ^{d} (b{b}) = {d} (b{b})\n", .{self.b, self.b, self.c, self.c, res, res});
        self.b = res;
    }

    fn out(self: CPU, operand: u8) !?u64 {
        const opval = try self.comboval(operand);
        const res = opval & 0b111;
        //dprint("out({d}): {d} & 0b111 = {d}\n", .{operand, opval, res});
        return res;
    }

    fn bdv(self: *CPU, operand: u8) !void {
        const opval = try self.comboval(operand);
        const denom = math.pow(u64, 2, opval);
        const res = self.a/denom;
        //dprint("bdv({d}): {d}/2^{d} -> {d}/{d} = {d}\n", .{operand, self.a, opval, self.a, denom, res});
        self.b = res;
    }

    fn cdv(self: *CPU, operand: u8) !void {
        const opval = try self.comboval(operand);
        const denom = math.pow(u64, 2, opval);
        const res = self.a/denom;
        //dprint("cdv({d}): {d}/2^{d} -> {d}/{d} = {d}\n", .{operand, self.a, opval, self.a, denom, res});
        self.c = res;
    }
};

fn countBits(val: u64) u6 {
    var tmp = val;
    var bits: u6 = 0;
    while (tmp > 0) {
        tmp /= 2;
        bits += 1;
    }
    return bits;
}

fn find(init_cpu: CPU, a: u64, testi: usize, bits_done: u6) !?u64 {
    var memi: usize = 0;
    var done_mask: u64 = 0;
    if (bits_done > 0) {
        done_mask = (@as(u64,@intCast(1)) << bits_done)-1;
    }
    for (0..std.math.maxInt(u16)) |i| {
        //const inita = (((inita >> bits_done)+1)<<bits_done) | (inita & ((@as(u64,@intCast(1))<<bits_done)-1));
        const inita = (i << bits_done) | (a & done_mask);
        //for (0..testi) |_| { dprint(" ", .{}); }
        //dprint("find(..., {d}, {d}, {d}) - testing: {d} (b{b})\n", .{a, testi, bits_done, inita, inita});
        var cpu = CPU {
            .a   = inita,
            .b   = init_cpu.b,
            .c   = init_cpu.b,
            .ip  = 0,
            .mem = init_cpu.mem,
        };
        while (true) {
            const output_op = cpu.step() catch |err| switch (err) {
                error.Halt => {
                    if (memi == cpu.mem.len) return inita;
                    break;
                },
                else => return err,
            };
            if (output_op) |output| {
                if (output != cpu.mem[memi]) { break; }
                if (inita != a and memi > testi) {
                    if (try find(init_cpu, inita, testi+1, bits_done+3)) |found| { return found; }
                    break;
                }
                memi += 1;
                if (memi == cpu.mem.len) return inita;
            }
        }
    }
    return null;
}

fn solve(reader: anytype) !u64 {
    const input = try reader.readAllAlloc(gpa(), std.math.maxInt(usize));
    var lines_it = mem.splitScalar(u8, input, '\n');
    //dprint("debug:\n", .{});
    const rega = try parseInt(u64, lines_it.next().?["Register A: ".len..]);
    const regb = try parseInt(u64, lines_it.next().?["Register B: ".len..]);
    const regc = try parseInt(u64, lines_it.next().?["Register C: ".len..]);
    _ = lines_it.next().?;
    const mem_str = lines_it.next().?["Program: ".len..];
    var mem_it = mem.splitScalar(u8, mem_str, ',');
    var mem_buf = try gpa().alloc(u8, (mem_str.len/2)+1);
    for (0..mem_buf.len) |i| { mem_buf[i] = try parseInt(u8, mem_it.next().?); }
    const init_cpu = CPU {
        .a   = rega,
        .b   = regb,
        .c   = regc,
        .ip  = 0,
        .mem = mem_buf,
    };
    return try find(init_cpu, 0, 0, 0) orelse return error.NotFound;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example2" { try test_solve(117440, "./17/example2.txt"); } // 11100101011000000
//test "input"    { try test_solve(0, "./17/input.txt"); }
