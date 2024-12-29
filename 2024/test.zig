const std = @import("std");
const dprint = std.debug.print;

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

pub fn main() !void {
    dprint("count: {b} sim {b} => {d}\n", .{1,2, countSimilarBits(1,2,2)});
    dprint("count: {b} sim {b} => {d}\n", .{1,3, countSimilarBits(1,3,2)});
    dprint("count: {b} sim {b} => {d}\n", .{0b1010101, 0b0101010, countSimilarBits(0b1010101, 0b0101010, 7)});
    dprint("count: {b} sim {b} => {d}\n", .{0b1010101, 0b1111110, countSimilarBits(0b1010101, 0b1111110, 7)});
    //const bits_goal: u8 = 46;
    //const max_val: usize = @as(usize, @intCast(1)) << @as(u6, @intCast(bits_goal)) - 1;
    //dprint("{d} + {d} = {d} == {any}\n", .{max_val/2, max_val/2, max_val,
    //    ((max_val/2)+(max_val/2) == max_val)});
    //dprint("{d} xor {d} = {d}\n", .{42, 15, 42^15});
    //dprint("{d} mod {d} = {d}\n", .{100000000, 16777216, @mod(100000000, 16777216)});
    //const nums = [_]u3{2,4,1,1,7,5,4,4,1,4,0,3,5,5,3,0};
    //for (nums) |num| {
    //    dprint("{d} (b{b:_>3}), xor: {d} (b{b}), neg: {d} (b{b})\n", .{
    //        num, num,
    //        num^num, num^num,
    //        ~num, ~num });
    //}
    //for (0..nums.len) |i| {
    //    dprint("{b:0>3}_", .{nums[nums.len-i-1]});
    //}
    //dprint("\n", .{});
}

