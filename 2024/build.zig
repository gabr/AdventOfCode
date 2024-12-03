const std = @import("std");

pub fn build(b: *std.Build) void {
    const args = b.args orelse {
        std.debug.print("pass task number as an argument to build, i.e.: zig build 03a\n", .{});
        std.process.exit(1);
    };
    const name = args[0];
    var buf: [255]u8 = undefined;
    const source_file = std.fmt.bufPrint(&buf, "{s}/{s}.zig", .{name[0..2], name}) catch unreachable;
    const path = b.path(source_file);
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
