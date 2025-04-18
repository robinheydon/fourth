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

    module.trace = false;

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

        if (std.mem.eql(u8, buffer.items, ".trace")) {
            module.trace = true;
            continue;
        }

        if (std.mem.eql(u8, buffer.items, ".exec")) {
            m.trace_execute = true;
            continue;
        }

        if (std.mem.eql(u8, buffer.items, ".dump")) {
            try m.dump(writer);
            try m.dump_module(module, writer);
            continue;
        }

        if (module.trace) {
            var token_iter = moon.tokenize(buffer.items);
            while (token_iter.next()) |tk| {
                std.debug.print("{s} \"{}\"\n", .{
                    @tagName(tk.kind),
                    std.zig.fmtEscapes(tk.get_string(buffer.items)),
                });
            }
        }

        if (module.trace) {
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
            m.clear_stack();

            var iter = moon.tokenize(buffer.items);
            var ast = m.AST(buffer.items);
            defer ast.deinit();

            const root = ast.parse(&iter) catch |err|
                {
                    try writer.print("Syntax error: {s}\n", .{@errorName(err)});
                    continue;
                };

            module.semantic_analysis(&ast, root) catch |err|
                {
                    try writer.print("Semantic error: {s}\n", .{@errorName(err)});
                    continue;
                };

            const module_func = module.generate_module_function(&ast, root) catch |err|
                {
                    try writer.print("Code generation error: {s}\n", .{@errorName(err)});
                    continue;
                };

            if (module.trace) {
                try m.dump_module(module, writer);
            }

            module.execute(module_func) catch |err|
                {
                    try writer.print("Execute error: {s}\n", .{@errorName(err)});
                    continue;
                };

            const value = m.pop() catch |err|
                {
                    if (err == error.StackUnderflow) {} else {
                        try writer.print("Stack error: {s}\n", .{@errorName(err)});
                    }
                    continue;
                };

            try m.write_value(value, writer);
            try writer.print("\n", .{});
        }
    }
}
