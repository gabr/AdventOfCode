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
        dprint("running opcode {d} with operand {d} (ip: {d})\n", .{opcode, operand, self.ip});
        switch (opcode) {
            0 => {        try self.adv(operand); return null; },
            1 => {        try self.bxl(operand); return null; },
            2 => {        try self.bst(operand); return null; },
            3 => {        try self.jnz(operand); return null; },
            4 => {        try self.bxc(operand); return null; },
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
        dprint ("adv({d}): {d}/2^{d} -> {d}/{d} = {d}\n", .{operand, self.a, opval, self.a, denom, res});
        self.a = res;
    }

    fn bxl(self: *CPU, operand: u8) !void {
        const opval: u64 = @intCast(operand);
        const res = self.b ^ opval;
        dprint ("bxl({d}): {d} (b{b})^{d} (b{b}) -> {d} (b{b})\n", .{operand, self.b, self.b, opval, opval, res, res});
        self.b = res;
    }

    fn bst(self: *CPU, operand: u8) !void {
        const opval = try self.comboval(operand);
        const res = opval & 0b111;
        dprint ("bst({d}): {d} (b{b}) & 0b111 -> {d} (b{b})\n", .{operand, opval, opval, res, res});
        self.b = res;
    }

    fn jnz(self: *CPU, operand: u8) !void {
        if (self.a == 0) {
            dprint ("jnz({d}): a == 0 - no jump\n", .{operand});
            return;
        }
        const opval: usize = @intCast(operand);
        dprint ("jnz({d}): ip: {d} -> {d}\n", .{operand, self.ip, opval});
        self.ip = opval;
    }

    fn bxc(self: *CPU, operand: u8) !void {
        const res = self.b ^ self.c;
        dprint ("bxc({d}): {d} (b{b}) ^{d} (b{b}) = {d} (b{b})\n", .{operand, self.b, self.b, self.c, self.c, res, res});
        self.b = res;
    }

    fn out(self: CPU, operand: u8) !?u64 {
        const opval = try self.comboval(operand);
        const res = opval & 0b111;
        dprint ("out({d}): {d} & 0b111 = {d}\n", .{operand, opval, res});
        return res;
    }

    fn bdv(self: *CPU, operand: u8) !void {
        const opval = try self.comboval(operand);
        const denom = math.pow(u64, 2, opval);
        const res = self.a/denom;
        dprint ("bdv({d}): {d}/2^{d} -> {d}/{d} = {d}\n", .{operand, self.a, opval, self.a, denom, res});
        self.b = res;
    }

    fn cdv(self: *CPU, operand: u8) !void {
        const opval = try self.comboval(operand);
        const denom = math.pow(u64, 2, opval);
        const res = self.a/denom;
        dprint ("cdv({d}): {d}/2^{d} -> {d}/{d} = {d}\n", .{operand, self.a, opval, self.a, denom, res});
        self.c = res;
    }
};

fn solve(reader: anytype) ![]u8 {
    const input = try reader.readAllAlloc(gpa(), std.math.maxInt(usize));
    var lines_it = mem.splitScalar(u8, input, '\n');
    dprint("debug:\n", .{});
    const rega = try parseInt(u64, lines_it.next().?["Register A: ".len..]);
    const regb = try parseInt(u64, lines_it.next().?["Register B: ".len..]);
    const regc = try parseInt(u64, lines_it.next().?["Register C: ".len..]);
    _ = lines_it.next().?;
    const mem_str = lines_it.next().?["Program: ".len..];
    var mem_it = mem.splitScalar(u8, mem_str, ',');
    var mem_buf = try gpa().alloc(u8, (mem_str.len/2)+1);
    for (0..mem_buf.len) |i| { mem_buf[i] = try parseInt(u8, mem_it.next().?); }
    var cpu = CPU {
        .a   = rega,
        .b   = regb,
        .c   = regc,
        .ip  = 0,
        .mem = mem_buf,
    };
    dprint("intial state:\n", .{});
    dprint(" reg a: {d}\n",   .{rega});
    dprint(" reg b: {d}\n",   .{regb});
    dprint(" reg c: {d}\n",   .{regc});
    dprint("    ip: {d}\n",   .{0});
    dprint(" mem: {any}\n",   .{mem_buf});
    const buf = try gpa().alloc(u8, 1024);
    var fbs = std.io.fixedBufferStream(buf);
    const output_writer = fbs.writer();
    while (true) {
        const output_op = cpu.step() catch |err| switch (err) {
            error.Halt => break,
            else => {
                dprint("Output so far: '{s}'\n", .{fbs.getWritten()});
                return err;
            },
        };
        if (output_op) |output| {
            try output_writer.print("{d},", .{output});
        }
    }
    fbs.pos -= 1; // remove last comma
    return fbs.getWritten();
}

fn test_solve(expected: []const u8, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqualSlices(u8, expected, try solve(file.reader()));
}
test "example1" { try test_solve("4,6,3,5,6,3,5,2,1,0", "./17/example1.txt"); }
test "input"    { try test_solve("7,4,2,0,5,0,5,3,7", "./17/input.txt"); }
