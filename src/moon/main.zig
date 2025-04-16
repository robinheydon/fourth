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

    var module = try m.create_module();
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    module.trace = true;

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    while (true) {
        try writer.print("moon> ", .{});

        buffer.shrinkRetainingCapacity(0);

        reader.streamUntilDelimiter(
            buffer.writer(),
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

        if (true) {
            var token_iter = moon.tokenize(buffer.items);
            while (token_iter.next()) |tk| {
                std.debug.print("{s} \"{}\"\n", .{
                    @tagName(tk.kind),
                    std.zig.fmtEscapes(tk.get_string(buffer.items)),
                });
            }
        }

        if (true) {
            var iter = moon.tokenize(buffer.items);
            var ast = m.AST(buffer.items);
            defer ast.deinit();

            const root = ast.parse(&iter) catch |err|
                {
                    try writer.print("Syntax error: {s}\n", .{@errorName(err)});
                    continue;
                };

            try ast.dump(root, .{}, writer);
        }

        if (true) {
            var iter = moon.tokenize(buffer.items);
            var ast = m.AST(buffer.items);
            defer ast.deinit();

            const root = ast.parse(&iter) catch |err|
                {
                    try writer.print("Syntax error: {s}\n", .{@errorName(err)});
                    continue;
                };

            try module.semantic_analysis(&ast, root);

            try module.generate_module_code_block(&ast, root);

            try m.dump_module(module, writer);
        }
    }
}
