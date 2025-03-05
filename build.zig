///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    ///////////////////////////////////////////////////////////////////////////////////////////

    const ng_mod = b.createModule(.{
        .root_source_file = b.path("src/ng/ng.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    ng_mod.addImport("ng", ng_mod);

    ///////////////////////////////////////////////////////////////////////////////////////////

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    exe_mod.addImport("ng", ng_mod);

    ///////////////////////////////////////////////////////////////////////////////////////////

    const exe = b.addExecutable(.{
        .name = "fourth",
        .root_module = exe_mod,
        .use_llvm = false,
    });

    b.installArtifact(exe);

    ///////////////////////////////////////////////////////////////////////////////////////////

    const check_format_step = try b.allocator.create(std.Build.Step);
    check_format_step.* = std.Build.Step.init(.{
        .id = std.Build.Step.Id.custom,
        .name = "check format",
        .owner = exe.step.owner,
        .makeFn = check_format,
    });
    exe.step.dependOn(check_format_step);

    ///////////////////////////////////////////////////////////////////////////////////////////

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    ///////////////////////////////////////////////////////////////////////////////////////////

    const ng_mod_test = b.createModule(.{
        .root_source_file = b.path("src/ng/ng.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
        .stack_protector = true,
        .omit_frame_pointer = false,
        .error_tracing = true,
    });
    ng_mod_test.addImport("ng", ng_mod_test);

    const ng_test = b.addTest(.{
        .name = "test",
        .root_module = ng_mod_test,
        .test_runner = .{
            .path = b.path("tools/test_runner.zig"),
            .mode = .simple,
        },
    });

    const test_run = b.addRunArtifact(ng_test);
    test_run.step.dependOn(check_format_step);

    const test_step = b.step("test", "Test the ng");
    test_step.dependOn(&test_run.step);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn check_format(step: *std.Build.Step, options: std.Build.Step.MakeOptions) anyerror!void {
    _ = options;

    const cwd = std.fs.cwd();
    const dir = try cwd.openDir(".", .{
        .iterate = true,
    });
    const b = step.owner;

    var had_error: bool = false;
    var iter = try dir.walk(b.allocator);
    while (try iter.next()) |entry| {
        if (entry.kind == .file) {
            if (std.mem.startsWith(u8, entry.path, ".zig-cache")) {
                continue;
            }

            if (std.mem.endsWith(u8, entry.path, ".zig")) {
                const file = try cwd.openFile(entry.path, .{});
                defer file.close();

                const content = try file.readToEndAlloc(b.allocator, std.math.maxInt(usize));
                defer b.allocator.free(content);

                var lines = std.mem.splitScalar(u8, content, '\n');
                var line_number: usize = 1;
                while (lines.next()) |line| {
                    if (line.len > 95) {
                        had_error = true;
                        const trimmed_line = std.mem.trim(u8, line, " ");
                        try step.addError("Line too long: {s}:{}\n  {s}", .{
                            entry.path,
                            line_number,
                            trimmed_line,
                        });
                    }
                    line_number += 1;
                }
            }
        }
    }

    if (had_error) return error.LineTooLong;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
