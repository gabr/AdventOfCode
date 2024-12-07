const std = @import("std");
const dprint = std.debug.print;

pub fn build(b: *std.Build) void {
    const args = b.args orelse {
        dprint("provide zig source file path, i.e.: zig build -- 03/part1.zig\n", .{});
        std.process.exit(1);
    };
    const zig_file_path = args[0];
    const path = b.path(zig_file_path);
    const name = i_will_make_a_name_out_of_you(zig_file_path);
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const exe = b.addExecutable(.{
        .name = name,
        .root_source_file = path,
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(exe);
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    //if (b.args) |args| { run_cmd.addArgs(args); }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
    const exe_unit_tests = b.addTest(.{
        .root_source_file = path,
        .target = target,
        .optimize = optimize,
    });
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
}

var name_buf: [std.fs.max_path_bytes]u8 = undefined;
/// takes a zig source file path and makes it a valid executable name
fn i_will_make_a_name_out_of_you(zig_source_path: []const u8) []const u8 {
    const ext = ".zig";
    if (!std.mem.endsWith(u8, zig_source_path, ext)) {
        dprint("provided zig source path has invalid extension: '{s}' (expected {s} at the end)",
            .{zig_source_path, ext});
        std.process.exit(1);
    }
    var name = name_buf[0..zig_source_path.len-ext.len];
    for (0..name.len) |i| {
        if (zig_source_path[i] == '/' or zig_source_path[i] == '\\') {
            name[i] = '_';
        } else {
            name[i] = zig_source_path[i];
        }
    }
    return name;
}
