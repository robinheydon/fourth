const std = @import("std");
const builtin = @import("builtin");

pub fn main() void {
    // @disableInstrumentation();

    const test_functions = builtin.test_functions;

    var passed: usize = 0;
    var skipped: usize = 0;
    var leaked: usize = 0;
    var failed: usize = 0;

    const root_node = std.Progress.start(.{
        .root_name = "Test",
        .estimated_total_items = test_functions.len,
    });

    for (1.., test_functions) |i, test_fn| {
        std.testing.allocator_instance = .{};
        std.testing.log_level = .debug;

        const test_node = root_node.start(test_fn.name, 0);

        if (test_fn.func()) |_| {
            passed += 1;
            std.debug.print("{}/{} OK   \"{}\"\n", .{
                i,
                test_functions.len,
                std.zig.fmtEscapes(test_fn.name),
            });
        } else |err| {
            switch (err) {
                error.SkipZigTest => {
                    skipped += 1;
                    std.debug.print("{}/{} SKIP \"{}\"\n", .{
                        i,
                        test_functions.len,
                        std.zig.fmtEscapes(test_fn.name),
                    });
                },
                else => {
                    failed += 1;
                    std.debug.print("{}/{} FAIL \"{}\" {s}\n", .{
                        i,
                        test_functions.len,
                        std.zig.fmtEscapes(test_fn.name),
                        @errorName(err),
                    });
                    if (@errorReturnTrace()) |trace| {
                        std.debug.dumpStackTrace(trace.*);
                    }
                },
            }
        }

        test_node.end();

        if (std.testing.allocator_instance.deinit() == .leak) {
            leaked += 1;
            std.debug.print("{}/{} LEAK \"{}\"\n", .{
                i,
                test_functions.len,
                std.zig.fmtEscapes(test_fn.name),
            });
        }
    }

    root_node.end();

    if (passed == test_functions.len) {
        std.debug.print("{} passed\n", .{passed});
    } else if (passed + skipped == test_functions.len) {
        std.debug.print("{} passed ({} skipped)\n", .{ passed, skipped });
    } else if (leaked > 0) {
        std.debug.print("{} leaked\n", .{failed});
    } else if (failed > 0) {
        std.debug.print("{} failed\n", .{failed});
    }

    if (leaked != 0 or failed != 0) {
        std.process.exit(1);
    }

    std.process.exit(0);
}
