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
        //dprint("adv({d}): {d}/2^{d} -> {d}/{d} = {d}\n", .{operand, self.a, opval, self.a, denom, res});
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


fn findStart(init_cpu: CPU) !u64 {
    var count_output: usize = 0;
    var i: u64 = 10;
    while (true) {
        var cpu = CPU {
            .a   = i,
            .b   = init_cpu.b,
            .c   = init_cpu.c,
            .ip  = 0,
            .mem = init_cpu.mem,
        };
        count_output = 0;
        while (true) {
            const output_op = cpu.step() catch |err| switch (err) {
                error.Halt => break,
                else => return err,
            };
            if (output_op) |_| { count_output += 1; }
        }
        if (count_output == init_cpu.mem.len) return i;
        i = i << 1;
    }
    unreachable;
}

fn solve(reader: anytype) !u64 {
    const input = try reader.readAllAlloc(gpa(), std.math.maxInt(usize));
    var lines_it = mem.splitScalar(u8, input, '\n');
    //dprint("debug:\n", .{});
             _ = try parseInt(u64, lines_it.next().?["Register A: ".len..]);
    const regb = try parseInt(u64, lines_it.next().?["Register B: ".len..]);
    const regc = try parseInt(u64, lines_it.next().?["Register C: ".len..]);
    _ = lines_it.next().?;
    const mem_str = lines_it.next().?["Program: ".len..];
    var mem_it = mem.splitScalar(u8, mem_str, ',');
    var mem_buf = try gpa().alloc(u8, (mem_str.len/2)+1);
    var out_buf = try gpa().alloc(u64, (mem_str.len/2)+1);
    for (0..mem_buf.len) |i| { mem_buf[i] = try parseInt(u8, mem_it.next().?); }
    var inita: u64 = try findStart(.{
        .a   = 0,
        .b   = regb,
        .c   = regc,
        .ip  = 0,
        .mem = mem_buf,
    });
    dprint("start at : {d} (b{b})\n", .{inita, inita});
    var memi: usize = 0;
    var memi_max: usize = 0;
    while (true) {
        //dprint("testing a: {d}", .{inita});
        memi = 0;
        var cpu = CPU {
            .a   = inita,
            .b   = regb,
            .c   = regc,
            .ip  = 0,
            .mem = mem_buf,
        };
        //dprint("intial state:\n", .{});
        //dprint(" reg a: {d}\n",   .{inita});
        //dprint(" reg b: {d}\n",   .{regb});
        //dprint(" reg c: {d}\n",   .{regc});
        //dprint("    ip: {d}\n",   .{0});
        //dprint(" mem: {any}\n",   .{mem_buf});
        while (true) {
            const output_op = cpu.step() catch |err| switch (err) {
                error.Halt => {
                    if (memi == mem_buf.len) return inita;
                    break;
                },
                else => return err,
            };
            if (output_op) |output| {
                out_buf[memi] = output;
                if (output != mem_buf[memi]) {
                    if (memi >= memi_max) {
                        dprint("inita: {d: >16} (b{b:_>64}), memi: {d}, mem: {any}\n", .{inita, inita, memi, out_buf[0..memi]});
                        memi_max = memi;
                    }
                    break;
                }
                memi += 1;
                if (memi == mem_buf.len) {
                    dprint("inita: {d: >16} (b{b:_>64}), memi: {d}, mem: {any}\n", .{inita, inita, memi, out_buf[0..memi]});
                    return inita;
                }
            }
        }
        inita += 1;
    }
    unreachable;
}

fn test_solve(expected: u64, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "example2" { try test_solve(117440, "./17/example2.txt"); }
//test "input"    { try test_solve(0, "./17/input.txt"); }
