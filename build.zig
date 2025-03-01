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

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const ng_mod = b.createModule(.{
        .root_source_file = b.path("src/ng/ng.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    ng_mod.addImport("ng", ng_mod);

    exe_mod.addImport("ng", ng_mod);

    const exe = b.addExecutable(.{
        .name = "fourth",
        .root_module = exe_mod,
    });

    const check_format_step = try b.allocator.create(std.Build.Step);
    check_format_step.* = std.Build.Step.init(.{
        .id = std.Build.Step.Id.custom,
        .name = "check format",
        .owner = exe.step.owner,
        .makeFn = check_format,
    });
    exe.step.dependOn(check_format_step);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
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

    var failed = false;

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
                        std.log.err("{s}:{} - too long {}\n{s}", .{
                            entry.path,
                            line_number,
                            line.len,
                            line,
                        });
                        failed = true;
                    }
                    line_number += 1;
                }
            }
        }
    }

    if (failed) return error.LinesTooLong;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
