const std = @import("std");
const moon = @import("moon");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    const stdout = std.io.getStdOut();
    const writer = stdout.writer();
    const stdin = std.io.getStdIn();
    const reader = stdin.reader();

    var m = moon.init(allocator);
    defer m.deinit();

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit ();

    while (true) {
        try writer.print("moon> ", .{});

        buffer.shrinkRetainingCapacity (0);

        reader.streamUntilDelimiter(
            buffer.writer (),
            '\n',
            std.math.maxInt(u32),
        ) catch |err| {
            switch (err) {
                error.EndOfStream => {
                    try writer.print("\n", .{});
                    break;
                },
                else => {
                    std.debug.print("{}\n", .{err});
                    break;
                },
            }
        };

        try writer.print("{} \"{}\"\n", .{
            buffer.items.len,
            std.zig.fmtEscapes(buffer.items),
        });
    }
}
