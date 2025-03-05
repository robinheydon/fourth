const std = @import("std");
const builtin = @import("builtin");

pub fn main() void {
    // @disableInstrumentation();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const test_functions = builtin.test_functions;

    var passed: usize = 0;
    var skipped: usize = 0;
    var leaked: usize = 0;
    var failed: usize = 0;

    var happy_buffer = std.ArrayList(u8).init(allocator);

    var happy_writer = happy_buffer.writer();

    const root_node = std.Progress.start(.{
        .root_name = "Test",
        .estimated_total_items = test_functions.len,
    });

    for (1.., test_functions) |i, test_fn| {
        std.testing.allocator_instance = .{};
        std.testing.log_level = .debug;

        const test_node = root_node.start(test_fn.name, 0);

        if (test_fn.func()) |_| {
            if (std.testing.allocator_instance.deinit() == .leak) {
                leaked += 1;
                std.debug.print("\x1b[31;1m{}/{} LEAK \"{}\"\x1b[0m\n", .{
                    i,
                    test_functions.len,
                    std.zig.fmtEscapes(test_fn.name),
                });
            } else {
                passed += 1;
                happy_writer.print("\x1b[32m{}/{} OK   \"{}\"\x1b[0m\n", .{
                    i,
                    test_functions.len,
                    std.zig.fmtEscapes(test_fn.name),
                }) catch {};
            }
        } else |err| {
            if (std.testing.allocator_instance.deinit() == .leak) {
                leaked += 1;
                std.debug.print("\x1b[31;1m{}/{} LEAK \"{}\"\x1b[0m\n", .{
                    i,
                    test_functions.len,
                    std.zig.fmtEscapes(test_fn.name),
                });
            } else {
                switch (err) {
                    error.SkipZigTest => {
                        skipped += 1;
                        happy_writer.print("\x1b[37;3m{}/{} SKIP \"{}\"\x1b[0m\n", .{
                            i,
                            test_functions.len,
                            std.zig.fmtEscapes(test_fn.name),
                        }) catch {};
                    },
                    else => {
                        failed += 1;
                        std.debug.print("\x1b[31;1m{}/{} FAIL \"{}\" {s}\x1b[0m\n", .{
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
        }

        test_node.end();
    }

    root_node.end();

    if (passed == test_functions.len) {
        std.debug.print("{s}", .{happy_buffer.items});
        std.debug.print("{} passed\n", .{passed});
    } else if (passed + skipped == test_functions.len) {
        std.debug.print("{s}", .{happy_buffer.items});
        std.debug.print("{} passed ({} skipped)\n", .{ passed, skipped });
    } else if (leaked > 0) {
        std.debug.print("{} leaked\n", .{leaked});
    } else if (failed > 0) {
        std.debug.print("{} failed\n", .{failed});
    }

    if (leaked != 0 or failed != 0) {
        std.process.exit(1);
    }

    std.process.exit(0);
}
