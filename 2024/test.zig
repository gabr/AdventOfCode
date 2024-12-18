const std = @import("std");
const dprint = std.debug.print;

pub fn main() !void {
    const nums = [_]u3{2,4,1,1,7,5,4,4,1,4,0,3,5,5,3,0};
    for (nums) |num| {
        dprint("{d} (b{b:_>3}), xor: {d} (b{b}), neg: {d} (b{b})\n", .{
            num, num,
            num^num, num^num,
            ~num, ~num });
    }
    for (0..nums.len) |i| {
        dprint("{b:0>3}_", .{nums[nums.len-i-1]});
    }
    dprint("\n", .{});
}

