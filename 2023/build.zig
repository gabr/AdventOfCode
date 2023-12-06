const std       = @import("std");
const mem       = std.mem;
const log       = std.log;
const fs        = std.fs;
const debug     = std.debug;
const Allocator = std.mem.Allocator;

pub fn build(b: *std.Build) void {
    validateZigVersion(b, "0.12.0-dev.17");

    // extracted into separate function so I can handle potential errors just in one place
    addExecutablesFromDirectories(b) catch |err| {
        log.err("error while looking for source files: '{s}'", .{ @errorName(err) });
    };
}

// Iterate over directories in the build.zig location to find all .zig source
// files and compile them.  Each file will result in single executable in the
// zig-out/bin directory.  The zig-bin and zig-cache directories are ignored.
// The output binaries names are the same as the source file names.
//
// Also all files are added to the single `test` step so you can
// run all tests via a single `zig build test` command.
fn addExecutablesFromDirectories(b: *std.Build) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const test_step = b.step("test", "Run unit tests");

    var cwd = try b.build_root.handle.openDir(".", .{ .iterate = true });
    defer cwd.close();

    var cwd_it = cwd.iterate();
    while (try cwd_it.next()) |entry| {
        // process only directories and skip zig-* special ones
        if (entry.kind != .directory or mem.startsWith(u8, entry.name, "zig-")) {
            continue;
        }

        //log.info("{s}\\", .{ entry.name });
        var src_dir = try cwd.openDir(entry.name, .{ .iterate = true });
        defer src_dir.close();
        var src_dir_it = src_dir.iterate();
        while (try src_dir_it.next()) |src_entry| {
            if (src_entry.kind != .file or !mem.endsWith(u8, src_entry.name, ".zig")) {
                continue;
            }

            const source_path = try fs.path.join(allocator, &.{ entry.name, src_entry.name });
            debug.print("source: {s}\n", .{ source_path });

            const exe = b.addExecutable(.{
                .name = try allocator.dupe(u8, src_entry.name[0..(src_entry.name.len - ".zig".len)]),
                .root_source_file = .{ .path = source_path },
                .target = target,
                .optimize = optimize,
            });
            b.installArtifact(exe);
            const exe_unit_tests = b.addTest(.{
                .root_source_file = .{ .path = source_path },
                .target = target,
                .optimize = optimize,
            });
            const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
            test_step.dependOn(&run_exe_unit_tests.step);
        }
    }
}

/// Runs "zig version" and compares with given value
/// If versions are not equal it will halt the build process.
fn validateZigVersion(b: *std.Build, expected_version_prefix: []const u8) void {
    const version = mem.trimRight(u8, b.run(&[_][]const u8{ b.zig_exe, "version" }), "\n\t\r");
    if (false == mem.startsWith(u8, version, expected_version_prefix)) {
        debug.print(
            "\n" ++
            "  Stop, stop, stop!\n" ++
            "  You're going to take someone's eye out.\n" ++
            "  Besides, you're executing it wrong.\n" ++
            "  It's zig version: {s}*\n" ++
            "               not: {s}\n" ++
            "\n" ++
            "  Make sure to use the zig compiler in version: {s}\n\n",
            .{ expected_version_prefix, version, expected_version_prefix } );
        std.process.exit(1);
    }
}
