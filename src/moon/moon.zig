///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const CompileOptions = struct {
    trace_code: bool = false,
};

pub fn init(alloc: std.mem.Allocator) Moon {
    return Moon.init(alloc);
}

const StringContext = struct {
    fn hash(a: String) u64 {
        return @bitCast(a);
    }
    fn eql(a: String, b: String) bool {
        return a.hash() == b.hash();
    }
};

const CFunction = *const fn (self: *Moon) callconv(.c) bool;

pub const Moon = struct {
    allocator: std.mem.Allocator,

    modules: std.ArrayListUnmanaged(*Module) = .empty,

    named_modules: std.ArrayHashMapUnmanaged(
        String,
        ModuleIndex,
        StringContext,
        false,
    ) = .empty,

    tables: std.ArrayListUnmanaged(*Table) = .empty,

    strings: std.ArrayListUnmanaged(u8) = .empty,

    stack: std.ArrayListUnmanaged(Value) = .empty,

    tos: usize = 0,
    execute_limit: usize = 1000,

    trace_execute: bool = false,

    pub fn init(alloc: std.mem.Allocator) Moon {
        return .{
            .allocator = alloc,
        };
    }

    pub fn deinit(self: *Moon) void {
        for (self.modules.items) |module| {
            module.deinit();
            self.allocator.destroy(module);
        }
        self.modules.deinit(self.allocator);

        for (self.tables.items) |table| {
            table.deinit();
            self.allocator.destroy(table);
        }
        self.tables.deinit(self.allocator);

        self.strings.deinit(self.allocator);

        self.stack.deinit(self.allocator);
    }

    pub fn add_std_module(self: *Moon) MoonErrors!void {
        const module = try self.create_module_named("std");
        _ = try self.add_module(module);

        try module.add_global_cfunction(try self.intern_string("print"), moon_std_print);
        try module.add_global_cfunction(try self.intern_string("sqrt"), moon_std_sqrt);
    }

    pub fn push_integer(self: *Moon, value: i64) MoonErrors!void {
        try self.stack.append(self.allocator, .{ .integer = value });
    }

    pub fn push_number(self: *Moon, value: f64) MoonErrors!void {
        try self.stack.append(self.allocator, .{ .number = value });
    }

    pub fn op(self: *Moon, operation: Operation) MoonErrors!void {
        switch (operation) {
            .add => {
                if (self.stack.pop()) |lhs| {
                    if (self.stack.pop()) |rhs| {
                        const res = try self.add(lhs, rhs);
                        try self.stack.append(self.allocator, res);
                        return;
                    }
                }
                return error.StackUnderflow;
            },
            .sub => {
                if (self.stack.pop()) |lhs| {
                    if (self.stack.pop()) |rhs| {
                        const res = try self.sub(lhs, rhs);
                        try self.stack.append(self.allocator, res);
                        return;
                    }
                }
                return error.StackUnderflow;
            },
            .mul => {
                if (self.stack.pop()) |lhs| {
                    if (self.stack.pop()) |rhs| {
                        const res = try self.mul(lhs, rhs);
                        try self.stack.append(self.allocator, res);
                        return;
                    }
                }
                return error.StackUnderflow;
            },
            .div => {
                if (self.stack.pop()) |lhs| {
                    if (self.stack.pop()) |rhs| {
                        const res = try self.div(lhs, rhs);
                        try self.stack.append(self.allocator, res);
                        return;
                    }
                }
                return error.StackUnderflow;
            },
            .mod => {
                if (self.stack.pop()) |lhs| {
                    if (self.stack.pop()) |rhs| {
                        const res = try self.mod(lhs, rhs);
                        try self.stack.append(self.allocator, res);
                        return;
                    }
                }
                return error.StackUnderflow;
            },
        }
    }

    pub fn pop_integer(self: *Moon) MoonErrors!i64 {
        if (self.stack.pop()) |a| {
            switch (a) {
                .integer => |value| return value,
                else => return error.NotInteger,
            }
        }
        return error.StackUnderflow;
    }

    pub fn get_value_at(self: *const Moon, index: usize) ?Value {
        const stack_index = self.tos + index;
        if (stack_index < self.stack.items.len) {
            return self.stack.items[stack_index];
        }
        return null;
    }

    pub fn to_number(self: *const Moon, index: usize) ?f64 {
        if (self.get_value_at(index)) |value| {
            switch (value) {
                .integer, .literal_integer => |i| return @floatFromInt(i),
                .number, .literal_number => |n| return n,
                else => return null,
            }
        }
        return null;
    }

    pub fn to_string(self: *const Moon, index: usize) ?[]const u8 {
        if (self.get_value_at(index)) |value| {
            switch (value) {
                .string => |str| return self.get_string(str),
                else => return null,
            }
        }
        return null;
    }

    pub fn compile(
        self: *Moon,
        source: []const u8,
        options: CompileOptions,
    ) MoonErrors!Value {
        const module = try self.generate_module(source, options);
        module.is_stdin = true;

        const index = self.add_module(module);

        return index;
    }

    fn add_module(self: *Moon, module: *Module) !Value {
        const index = self.modules.items.len;
        try self.modules.append(self.allocator, module);

        return .{
            .module = @enumFromInt(index),
        };
    }

    pub fn create_module_named(self: *Moon, name: []const u8) MoonErrors!*Module {
        const name_string = try self.intern_string(name);
        const module = try self.create_module();
        module.set_name(name_string);
        return module;
    }

    pub fn create_module(self: *Moon) MoonErrors!*Module {
        const module = try self.allocator.create(Module);
        module.* = .{
            .moon = self,
            .name = .null_string,
            .path = .null_string,
            .globals = .empty,
            .trace = false,
        };
        return module;
    }

    fn generate_module(
        self: *Moon,
        source: []const u8,
        options: CompileOptions,
    ) MoonErrors!*Module {
        var iter = tokenize(source);

        var tree = self.AST(source);
        defer tree.deinit();

        const root = try tree.parse(&iter);

        if (options.trace_code) {
            var buffer = std.ArrayList(u8).init(self.allocator);
            defer buffer.deinit();

            const writer = buffer.writer();
            try tree.dump(root, .{
                .show_node_index = true,
            }, writer);
            std.debug.print("{s}\n", .{buffer.items});
        }

        var module = try self.create_module();
        errdefer {
            module.deinit();
            self.allocator.destroy(module);
        }

        module.trace = options.trace_code;

        try module.semantic_analysis(&tree, root);

        const module_func = try module.generate_module_function(&tree, root);
        defer {
            module_func.deinit();
            self.allocator.destroy(module_func);
        }

        for (module.globals.items) |*global| {
            switch (global.kind) {
                .function => {
                    const func_index = global.value.function;
                    const func = module.functions.items[@intFromEnum(func_index)];
                    if (func.need_generation) {
                        try module.generate_function(
                            func,
                            &tree,
                        );
                        func.need_generation = false;
                    }
                },
                else => {},
            }
        }

        if (module.trace) {
            var buffer = std.ArrayList(u8).init(std.testing.allocator);
            defer buffer.deinit();
            const writer = buffer.writer();
            try writer.print("  module_function\n", .{});

            try writer.print("    code\n", .{});
            try module_func.dump_code(3, writer);

            try writer.print("    constants\n", .{});
            for (module_func.constants.items, 0..) |constant, i| {
                try writer.print("      {}: ", .{i});
                try self.write_value(constant, writer);
                try writer.print("\n", .{});
            }
            std.debug.print("{s}\n", .{buffer.items});
        }

        try self.execute(module_func);

        return module;
    }

    pub fn AST(self: *Moon, source: []const u8) Moon_AST {
        return .{
            .moon = self,
            .source = source,
        };
    }

    pub fn clear_stack(self: *Moon) void {
        while (self.stack.items.len > 0) {
            _ = self.stack.pop();
        }
    }

    pub fn push(self: *Moon, value: Value) MoonErrors!void {
        try self.stack.append(self.allocator, value);
    }

    pub fn pop(self: *Moon) MoonErrors!Value {
        if (self.stack.pop()) |value| {
            return value;
        }
        return error.StackUnderflow;
    }

    pub fn get_relative(self: *Moon, offset: isize) MoonErrors!Value {
        const index = @as(isize, @intCast(self.stack.items.len)) + offset;
        if (index < 0) return error.StackUnderflow;
        if (index >= self.stack.items.len) return error.StackUnderflow;
        const value = self.stack.items[@intCast(index)];
        return value;
    }

    pub fn raise_type_mismatch(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        _ = self;
        std.debug.print("Type mismatch: {s} {s}\n", .{
            @tagName(lhs),
            @tagName(rhs),
        });
        return error.TypeMismatch;
    }

    pub fn add(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .integer = l + r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_integer => |l| {
                switch (rhs) {
                    .integer => |r| {
                        return .{ .integer = l + r };
                    },
                    .literal_integer => |r| {
                        return .{ .literal_integer = l + r };
                    },
                    .number => |r| {
                        const fl: f64 = @floatFromInt(l);
                        return .{ .number = fl + r };
                    },
                    .literal_number => |r| {
                        const fl: f64 = @floatFromInt(l);
                        return .{ .literal_number = fl + r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .number => |l| {
                switch (rhs) {
                    .number => |r| {
                        return .{ .number = l + r };
                    },
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .number = l + fr };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_number => |l| {
                switch (rhs) {
                    .number => |r| {
                        return .{ .number = l + r };
                    },
                    .literal_number => |r| {
                        return .{ .literal_number = l + r };
                    },
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .literal_number = l + fr };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn sub(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .integer = l - r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_integer => |l| {
                switch (rhs) {
                    .integer => |r| {
                        return .{ .integer = l - r };
                    },
                    .literal_integer => |r| {
                        return .{ .literal_integer = l - r };
                    },
                    .number => |r| {
                        const fl: f64 = @floatFromInt(l);
                        return .{ .number = fl - r };
                    },
                    .literal_number => |r| {
                        const fl: f64 = @floatFromInt(l);
                        return .{ .literal_number = fl - r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .number => |l| {
                switch (rhs) {
                    .number => |r| {
                        return .{ .number = l - r };
                    },
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .number = l - fr };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_number => |l| {
                switch (rhs) {
                    .number => |r| {
                        return .{ .number = l - r };
                    },
                    .literal_number => |r| {
                        return .{ .literal_number = l - r };
                    },
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .literal_number = l - fr };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn mul(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .integer = l * r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_integer => |l| {
                switch (rhs) {
                    .integer => |r| {
                        return .{ .integer = l * r };
                    },
                    .literal_integer => |r| {
                        return .{ .literal_integer = l * r };
                    },
                    .number => |r| {
                        const fl: f64 = @floatFromInt(l);
                        return .{ .number = fl * r };
                    },
                    .literal_number => |r| {
                        const fl: f64 = @floatFromInt(l);
                        return .{ .literal_number = fl * r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .number => |l| {
                switch (rhs) {
                    .number => |r| {
                        return .{ .number = l * r };
                    },
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .number = l * fr };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_number => |l| {
                switch (rhs) {
                    .number => |r| {
                        return .{ .number = l * r };
                    },
                    .literal_number => |r| {
                        return .{ .literal_number = l * r };
                    },
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .literal_number = l * fr };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn div(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .integer => |l| {
                const fl: f64 = @floatFromInt(l);
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .number = fl / fr };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_integer => |l| {
                const fl: f64 = @floatFromInt(l);
                switch (rhs) {
                    .integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .number = fl / fr };
                    },
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .literal_number = fl / fr };
                    },
                    .number => |r| {
                        return .{ .number = fl / r };
                    },
                    .literal_number => |r| {
                        return .{ .literal_number = fl / r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .number => |l| {
                switch (rhs) {
                    .number => |r| {
                        return .{ .number = l / r };
                    },
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .number = l / fr };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_number => |l| {
                switch (rhs) {
                    .number => |r| {
                        return .{ .number = l / r };
                    },
                    .literal_number => |r| {
                        return .{ .literal_number = l / r };
                    },
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .literal_number = l / fr };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn mod(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer => |r| {
                        return .{ .integer = @mod(l, r) };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .number => |l| {
                switch (rhs) {
                    .number => |r| {
                        return .{ .number = @mod(l, r) };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .integer = @mod(l, r) };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_integer => |l| {
                switch (rhs) {
                    .integer => |r| {
                        return .{ .integer = @mod(l, r) };
                    },
                    .literal_integer => |r| {
                        return .{ .literal_integer = @mod(l, r) };
                    },
                    .number => |r| {
                        const fl: f64 = @floatFromInt(l);
                        return .{ .number = fl % r };
                    },
                    .literal_number => |r| {
                        const fl: f64 = @floatFromInt(l);
                        return .{ .literal_number = fl / r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .number => |l| {
                switch (rhs) {
                    .number => |r| {
                        return .{ .number = l % r };
                    },
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .number = l % fr };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_number => |l| {
                switch (rhs) {
                    .number => |r| {
                        return .{ .number = l % r };
                    },
                    .literal_number => |r| {
                        return .{ .literal_number = l % r };
                    },
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .literal_number = l % fr };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn lsh(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .integer = l << @intCast(r) };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_integer => |l| {
                switch (rhs) {
                    .integer => |r| {
                        return .{ .integer = l << @intCast(r) };
                    },
                    .literal_integer => |r| {
                        return .{ .literal_integer = l << @intCast(r) };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn rsh(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .integer = l >> @intCast(r) };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_integer => |l| {
                switch (rhs) {
                    .integer => |r| {
                        return .{ .integer = l >> @intCast(r) };
                    },
                    .literal_integer => |r| {
                        return .{ .literal_integer = l >> @intCast(r) };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn band(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .integer = l & r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_integer => |l| {
                switch (rhs) {
                    .integer => |r| {
                        return .{ .integer = l & r };
                    },
                    .literal_integer => |r| {
                        return .{ .literal_integer = l & r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn bor(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .integer = l | r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_integer => |l| {
                switch (rhs) {
                    .integer => |r| {
                        return .{ .integer = l | r };
                    },
                    .literal_integer => |r| {
                        return .{ .literal_integer = l | r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn bxor(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .integer = l ^ r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_integer => |l| {
                switch (rhs) {
                    .integer => |r| {
                        return .{ .integer = l ^ r };
                    },
                    .literal_integer => |r| {
                        return .{ .literal_integer = l ^ r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn lt(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .boolean = l < r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .boolean = l < r };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        const fl: f64 = @floatFromInt(l);
                        return .{ .boolean = fl < r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .number => |l| {
                switch (rhs) {
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .boolean = l < fr };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        return .{ .boolean = l < r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_number => |l| {
                switch (rhs) {
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .boolean = l < fr };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        return .{ .boolean = l < r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn gt(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .boolean = l > r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .boolean = l > r };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        const fl: f64 = @floatFromInt(l);
                        return .{ .boolean = fl > r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .number => |l| {
                switch (rhs) {
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .boolean = l > fr };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        return .{ .boolean = l > r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_number => |l| {
                switch (rhs) {
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .boolean = l > fr };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        return .{ .boolean = l > r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn lte(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .boolean = l <= r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .boolean = l <= r };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        const fl: f64 = @floatFromInt(l);
                        return .{ .boolean = fl <= r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .number => |l| {
                switch (rhs) {
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .boolean = l <= fr };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        return .{ .boolean = l <= r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_number => |l| {
                switch (rhs) {
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .boolean = l <= fr };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        return .{ .boolean = l <= r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn gte(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .boolean = l >= r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .boolean = l >= r };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        const fl: f64 = @floatFromInt(l);
                        return .{ .boolean = fl >= r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .number => |l| {
                switch (rhs) {
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .boolean = l >= fr };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        return .{ .boolean = l >= r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_number => |l| {
                switch (rhs) {
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .boolean = l >= fr };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        return .{ .boolean = l >= r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn eq(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .boolean = l == r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .boolean = l == r };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        const fl: f64 = @floatFromInt(l);
                        return .{ .boolean = fl == r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .number => |l| {
                switch (rhs) {
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .boolean = l == fr };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        return .{ .boolean = l == r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_number => |l| {
                switch (rhs) {
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .boolean = l == fr };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        return .{ .boolean = l == r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn neq(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .boolean = l != r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_integer => |l| {
                switch (rhs) {
                    .integer,
                    .literal_integer,
                    => |r| {
                        return .{ .boolean = l != r };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        const fl: f64 = @floatFromInt(l);
                        return .{ .boolean = fl != r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .number => |l| {
                switch (rhs) {
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .boolean = l != fr };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        return .{ .boolean = l != r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            .literal_number => |l| {
                switch (rhs) {
                    .literal_integer => |r| {
                        const fr: f64 = @floatFromInt(r);
                        return .{ .boolean = l != fr };
                    },
                    .number,
                    .literal_number,
                    => |r| {
                        return .{ .boolean = l != r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn @"and"(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .boolean => |l| {
                switch (rhs) {
                    .boolean => |r| {
                        return .{ .boolean = l and r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn @"or"(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        switch (lhs) {
            .boolean => |l| {
                switch (rhs) {
                    .boolean => |r| {
                        return .{ .boolean = l or r };
                    },
                    else => return self.raise_type_mismatch(lhs, rhs),
                }
            },
            else => return self.raise_type_mismatch(lhs, rhs),
        }
    }

    pub fn get_string(self: Moon, string: String) []const u8 {
        const str: StringInternal = @bitCast(@intFromEnum(string));
        return self.strings.items[str.offset .. str.offset + str.len];
    }

    pub fn intern_string(self: *Moon, str: []const u8) MoonErrors!String {
        const len: u32 = @intCast(str.len);
        if (std.mem.indexOf(u8, self.strings.items, str)) |uoffset| {
            const offset: u32 = @intCast(uoffset);
            const istr: StringInternal = .{
                .offset = offset,
                .len = len,
            };
            return @enumFromInt(@as(u64, @bitCast(istr)));
        }

        const offset: u32 = @intCast(self.strings.items.len);
        try self.strings.appendSlice(self.allocator, str);
        const istr: StringInternal = .{
            .offset = offset,
            .len = len,
        };
        return @enumFromInt(@as(u64, @bitCast(istr)));
    }

    pub fn execute(
        self: *Moon,
        func: *Function,
    ) MoonErrors!void {
        var counter: usize = 0;
        var ip: isize = 0;
        const writer = std.io.getStdErr().writer();
        if (self.trace_execute) {
            try writer.print("Function : [", .{});
            try self.dump_stack(writer);
            try writer.print("]\n", .{});
        }
        while (ip < func.code.items.len) {
            counter += 1;
            if (counter > self.execute_limit) {
                return error.ExecuteLimit;
            }
            const instr = func.code.items[@intCast(ip)];
            if (self.trace_execute) {
                try func.dump_instruction(@intCast(ip), instr, 1, writer);
            }
            ip += 1;
            switch (instr.len()) {
                .short => {
                    const code = instr.get_short();
                    switch (code.op) {
                        .constant => {
                            const constant = func.constants.items[code.arg];
                            try self.push(constant);
                        },
                        .jmp => {
                            ip += code.arg;
                        },
                        .jmpk => {
                            const constant = func.constants.items[code.arg];
                            switch (constant) {
                                .integer => |i| {
                                    ip += @as(i32, @intCast(i));
                                },
                                else => {
                                    return error.InvalidInstruction;
                                },
                            }
                        },
                        .bra => {
                            const truthy = try self.pop();
                            if (truthy.is_true()) {
                                ip += code.arg;
                            }
                        },
                        .brak => {
                            const constant = func.constants.items[code.arg];
                            const truthy = try self.pop();
                            if (truthy.is_true()) {
                                switch (constant) {
                                    .integer => |i| {
                                        ip += @as(i32, @intCast(i));
                                    },
                                    else => {
                                        return error.InvalidInstruction;
                                    },
                                }
                            }
                        },
                    }
                },
                .long => {
                    const code = instr.get_long();
                    switch (code.op) {
                        .nop => {},
                        .nil => {
                            try self.push(.{ .nil = {} });
                        },
                        .boolean_true => {
                            try self.push(.{ .boolean = true });
                        },
                        .boolean_false => {
                            try self.push(.{ .boolean = false });
                        },
                        .integer => {
                            try self.push(.{ .literal_integer = code.arg });
                        },
                        .add => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.add(lhs, rhs);
                            try self.push(result);
                        },
                        .sub => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.sub(lhs, rhs);
                            try self.push(result);
                        },
                        .mul => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.mul(lhs, rhs);
                            try self.push(result);
                        },
                        .div => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.div(lhs, rhs);
                            try self.push(result);
                        },
                        .mod => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.mod(lhs, rhs);
                            try self.push(result);
                        },
                        .lsh => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.lsh(lhs, rhs);
                            try self.push(result);
                        },
                        .rsh => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.rsh(lhs, rhs);
                            try self.push(result);
                        },
                        .band => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.band(lhs, rhs);
                            try self.push(result);
                        },
                        .bor => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.bor(lhs, rhs);
                            try self.push(result);
                        },
                        .bxor => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.bxor(lhs, rhs);
                            try self.push(result);
                        },
                        .lt => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.lt(lhs, rhs);
                            try self.push(result);
                        },
                        .gt => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.gt(lhs, rhs);
                            try self.push(result);
                        },
                        .lte => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.lte(lhs, rhs);
                            try self.push(result);
                        },
                        .gte => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.gte(lhs, rhs);
                            try self.push(result);
                        },
                        .eq => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.eq(lhs, rhs);
                            try self.push(result);
                        },
                        .neq => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.neq(lhs, rhs);
                            try self.push(result);
                        },
                        .@"and" => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.@"and"(lhs, rhs);
                            try self.push(result);
                        },
                        .@"or" => {
                            const rhs = try self.pop();
                            const lhs = try self.pop();
                            const result = try self.@"or"(lhs, rhs);
                            try self.push(result);
                        },
                        .drop => {
                            const num = code.arg;
                            for (0..num) |_| {
                                _ = try self.pop();
                            }
                        },
                        .load_table_k => {
                            const lhs = try self.pop();
                            const rhs = func.constants.items[code.arg];
                            const value = self.table_get(lhs, rhs);
                            try self.push(value);
                        },
                        .store_table_k => {
                            return error.InvalidInstruction;
                        },
                        .load_local => {
                            const index = self.tos + code.arg;
                            try self.push(self.stack.items[index]);
                        },
                        .store_local => {
                            const value = try self.pop();
                            const index = self.tos + code.arg;
                            self.stack.items[index] = value;
                        },
                        .load_global => {
                            const global = func.module.globals.items[code.arg];
                            try self.push(global.value);
                        },
                        .store_global => {
                            const value = try self.pop();
                            func.module.globals.items[code.arg].value = value;
                        },
                        .call => {
                            try writer.print("\n", .{});
                            const diff = code.arg;
                            const args: ShortIArg = @intCast(diff);
                            const func_value = try self.get_relative(-args - 1);
                            switch (func_value) {
                                .function => |fi| {
                                    const func_index = @intFromEnum(fi);
                                    const callee = func.module.functions.items[func_index];
                                    const old_tos = self.tos;
                                    self.tos = self.stack.items.len - code.arg;
                                    try self.execute(callee);
                                    self.stack.items[self.tos - 1] = try self.pop();
                                    self.stack.items.len = self.tos;
                                    self.tos = old_tos;
                                },
                                .cfunction => |cf| {
                                    const old_tos = self.tos;
                                    self.tos = self.stack.items.len - code.arg;
                                    if (cf(self)) {
                                        self.stack.items[self.tos - 1] = try self.pop();
                                    } else {
                                        self.stack.items[self.tos - 1] = .nil;
                                    }
                                    self.stack.items.len = self.tos;
                                    self.tos = old_tos;
                                },
                                else => return error.InvalidFunction,
                            }
                        },
                        .ret => {
                            return;
                        },
                        .new_table => {
                            try self.new_table();
                        },
                        .append_table => {
                            return error.InvalidInstruction;
                        },
                        .import => {
                            try self.import();
                        },
                    }
                },
            }
            if (self.trace_execute) {
                try writer.print(" => [", .{});
                try self.dump_stack(writer);
                try writer.print("]\n", .{});
            }
        }
    }

    pub fn import(self: *Moon) MoonErrors!void {
        const value = try self.pop();
        const str = value.as_string() orelse .null_string;
        for (self.modules.items, 0..) |module, index| {
            if (module.name == str) {
                try self.push(value_from_module(@enumFromInt(index)));
                return;
            }
        }

        return error.ModuleUnknown;
    }

    pub fn new_table(self: *Moon) MoonErrors!void {
        const table = try self.allocator.create(Table);
        table.* = .{
            .moon = self,
        };
        const index = try self.add_table(table);
        try self.push(.{
            .table = index,
        });
    }

    pub fn add_table(self: *Moon, table: *Table) MoonErrors!TableIndex {
        const index = self.tables.items.len;
        try self.tables.append(self.allocator, table);
        return @enumFromInt(index);
    }

    pub fn table_get(self: *Moon, table: Value, index: Value) Value {
        if (table.is_module()) {
            return self.module_get(table, index);
        } else if (!table.is_table()) {
            return .nil;
        }

        return .nil;
    }

    pub fn module_get(self: *Moon, module_value: Value, key: Value) Value {
        if (module_value.as_module()) |module_index| {
            const mi: usize = @intFromEnum(module_index);
            const module = self.modules.items[mi];
            if (key.as_string()) |name| {
                if (module.get_global_value(name)) |value| {
                    return value;
                }
            }
            return .nil;
        } else {
            return .nil;
        }
    }

    pub fn write_value(self: *Moon, value: Value, writer: anytype) MoonErrors!void {
        switch (value) {
            .nil => try writer.print("nil", .{}),
            .boolean => |b| try writer.print("{}", .{b}),
            .integer,
            .literal_integer,
            => |i| try writer.print("{d}", .{i}),
            .number,
            .literal_number,
            => |n| try writer.print("{d}", .{n}),
            .string => |s| {
                const str = self.get_string(s);
                try writer.print("\"{}\"", .{std.zig.fmtEscapes(str)});
            },
            .function => |f| {
                const index = @intFromEnum(f);
                try writer.print("function#{}", .{index});
            },
            .cfunction => {
                try writer.print("cfunction:", .{});
            },
            .module => |m| {
                const index = @intFromEnum(m);
                try writer.print("module#{}", .{index});
            },
            .table => |t| {
                const index = @intFromEnum(t);
                try writer.print("table#{}", .{index});
            },
        }
    }

    pub fn dump(self: *Moon, writer: anytype) MoonErrors!void {
        try writer.print("moon\n", .{});
        if (false) {
            try writer.print("  strings\n", .{});
            for (0..self.strings.items.len) |index| {
                const start = index * 32;
                if (start >= self.strings.items.len) break;

                const end = @min(self.strings.items.len, index * 32 + 32);
                try writer.print("    {x:0>8} \"{}\"\n", .{
                    start,
                    std.zig.fmtEscapes(self.strings.items[start..end]),
                });
            }
        }
        try writer.print("  tables\n", .{});
        for (self.tables.items, 0..) |table, index| {
            try writer.print("    #{}:", .{index});
            try self.dump_table(table, writer);
            try writer.print("\n", .{});
        }
        try writer.print("  modules\n", .{});
        for (self.modules.items, 0..) |module, index| {
            try writer.print("    {}:", .{index});
            if (module.name != .null_string) {
                const name = self.get_string(module.name);
                try writer.print(" \"{}\"", .{std.zig.fmtEscapes(name)});
            } else if (module.is_stdin) {
                try writer.print(" stdin", .{});
            }
            try writer.print("\n", .{});
        }
    }

    pub fn dump_table(self: *Moon, table: *Table, writer: anytype) MoonErrors!void {
        _ = self;

        try writer.print(" ({},{})", .{ table.hash.count(), table.array.items.len });
    }

    pub fn dump_module(self: *Moon, module: *Module, writer: anytype) MoonErrors!void {
        try writer.print("module\n", .{});

        try writer.print("  globals\n", .{});
        for (module.globals.items, 0..) |global, index| {
            const name = self.get_string(global.name);
            try writer.print("    {}: \"{}\" {s}", .{
                index,
                std.zig.fmtEscapes(name),
                @tagName(global.kind),
            });
            try writer.print(" = ", .{});
            try self.write_value(global.value, writer);
            try writer.print("\n", .{});
        }

        for (module.functions.items, 0..) |func, index| {
            try writer.print("  function#{} ({} params)\n", .{
                index,
                func.num_params,
            });

            try writer.print("    code\n", .{});
            try func.dump_code(3, writer);

            try writer.print("    constants\n", .{});
            for (func.constants.items, 0..) |constant, i| {
                try writer.print("      {}: ", .{i});
                try self.write_value(constant, writer);
                try writer.print("\n", .{});
            }
        }
    }

    pub fn dump_stack(self: *Moon, writer: anytype) !void {
        for (self.stack.items[self.tos..], 0..) |value, index| {
            if (index > 0) {
                try writer.print(", ", .{});
            }
            try self.write_value(value, writer);
        }
    }

    pub fn debug_stack(self: *Moon, label: []const u8) void {
        var buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();
        const writer = buffer.writer();
        writer.print("debug_stack (\"{}\") => [", .{std.zig.fmtEscapes(label)}) catch {};
        self.dump_stack(writer) catch {};
        writer.print("]\n", .{}) catch {};
        std.debug.print("{s}", .{buffer.items});
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const InstShortLong = enum(u1) { short, long };

pub const Instruction = enum(u16) {
    _,

    pub fn len(self: Instruction) InstShortLong {
        const inst = self.get_long();
        return inst.kind;
    }

    pub fn short(op: ShortOpcode, arg: ShortArg) Instruction {
        const inst: ShortInstruction = .{
            .op = op,
            .arg = arg,
        };
        return @enumFromInt(@as(u16, @bitCast(inst)));
    }

    pub fn long(op: LongOpcode, arg: LongArg) Instruction {
        const inst: LongInstruction = .{
            .op = op,
            .arg = arg,
        };
        return @enumFromInt(@as(u16, @bitCast(inst)));
    }

    pub fn get_short(self: Instruction) ShortInstruction {
        return @bitCast(@intFromEnum(self));
    }

    pub fn get_long(self: Instruction) LongInstruction {
        return @bitCast(@intFromEnum(self));
    }
};

pub const ShortInstruction = packed struct(u16) {
    arg: ShortArg,
    op: ShortOpcode,
    kind: InstShortLong = .short,
};

pub const LongInstruction = packed struct(u16) {
    arg: LongArg,
    op: LongOpcode,
    kind: InstShortLong = .long,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const ShortOpcode = enum(u3) {
    jmp,
    jmpk,
    bra,
    brak,
    constant,
};

pub const ShortArg = u12;
pub const ShortIArg = i12;
pub const LongArg = u8;
pub const LongIArg = i8;

pub const LongOpcode = enum(u7) {
    nil,
    nop,
    drop,
    add,
    sub,
    mul,
    div,
    mod,
    lsh,
    rsh,
    band,
    bor,
    bxor,
    lt,
    gt,
    lte,
    gte,
    eq,
    neq,
    @"and",
    @"or",
    load_table_k,
    store_table_k,
    boolean_true,
    boolean_false,
    integer,
    load_local,
    store_local,
    load_global,
    store_global,
    call,
    ret,
    new_table,
    append_table,
    import,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const GlobalKind = enum {
    variable,
    constant,
    function,
};

const Global = struct {
    kind: GlobalKind,
    name: String,
    value: Value,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const LocalKind = enum {
    constant,
    variable,
};

const Local = struct {
    name: String,
    kind: LocalKind,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const Module = struct {
    moon: *Moon,
    name: String,
    path: String,
    is_stdin: bool = false,
    functions: std.ArrayListUnmanaged(*Function) = .empty,
    globals: std.ArrayListUnmanaged(Global),
    trace: bool = false,
    trace_execute: bool = false,

    pub fn deinit(self: *Module) void {
        for (self.functions.items) |function| {
            function.deinit();
            self.moon.allocator.destroy(function);
        }
        self.functions.deinit(self.moon.allocator);

        self.globals.deinit(self.moon.allocator);
    }

    pub fn set_name(self: *Module, name: String) void {
        self.name = name;
    }

    pub fn set_path(self: *Module, path: String) void {
        self.path = path;
    }

    pub fn semantic_analysis(
        self: *Module,
        tree: *const Moon_AST,
        node_index: NodeIndex,
    ) MoonErrors!void {
        const index = node_index.as_usize();
        const node = tree.nodes.items[index];
        switch (node) {
            .block => |stmts| {
                for (stmts.items) |stmt| {
                    try self.semantic_statement(tree, stmt);
                }
            },
            else => {
                // std.debug.print("No Semantic: {s}\n", .{@tagName(node)});
            },
        }
    }

    pub fn semantic_statement(
        self: *Module,
        tree: *const Moon_AST,
        node_index: NodeIndex,
    ) MoonErrors!void {
        const index = node_index.as_usize();
        const node = tree.nodes.items[index];
        switch (node) {
            .const_decl => |decl| {
                _ = try self.declare_global(decl.name, .constant);
            },
            .var_decl => |decl| {
                _ = try self.declare_global(decl.name, .variable);
            },
            .func_decl => |decl| {
                var func = try self.create_function();
                const func_index = try self.add_function(func);
                func.num_params = @intCast(decl.params.len);
                func.block = decl.block;
                for (decl.params) |param| {
                    const name = try self.moon.intern_string(param.name);
                    try func.declare_local(name, .constant);
                }
                const gi = try self.declare_global(decl.name, .function);
                self.globals.items[gi].value = .{ .function = func_index };
            },
            else => {
                // std.debug.print("Statement: {s}\n", .{@tagName(node)});
            },
        }
    }

    pub fn create_function(self: *Module) MoonErrors!*Function {
        const func = try self.moon.allocator.create(Function);
        func.* = .{
            .module = self,
        };
        return func;
    }

    pub fn add_global_cfunction(
        self: *Module,
        name: String,
        func: CFunction,
    ) MoonErrors!void {
        if (self.find_global(name)) |_| {
            std.debug.print("{s} already declared (declare global / global)\n", .{
                self.moon.get_string(name),
            });
            return error.NameAlreadyDeclared;
        }
        try self.globals.append(self.moon.allocator, .{
            .name = name,
            .kind = .constant,
            .value = value_from_cfunction(func),
        });
    }

    pub fn add_function(self: *Module, func: *Function) MoonErrors!FunctionIndex {
        const index: FunctionIndex = @enumFromInt(self.functions.items.len);
        try self.functions.append(self.moon.allocator, func);
        return index;
    }

    pub fn declare_global(
        self: *Module,
        name_string: []const u8,
        kind: GlobalKind,
    ) MoonErrors!u32 {
        const name = try self.moon.intern_string(name_string);
        if (self.find_global(name)) |_| {
            std.debug.print("{s} already declared (declare global / global)\n", .{
                name_string,
            });
            return error.NameAlreadyDeclared;
        }
        const index = self.globals.items.len;
        try self.globals.append(self.moon.allocator, .{
            .name = name,
            .kind = kind,
            .value = .{ .nil = {} },
        });
        return @intCast(index);
    }

    pub fn find_global(self: *Module, name: String) ?u32 {
        for (self.globals.items, 0..) |global, index| {
            if (global.name == name) return @intCast(index);
        }
        return null;
    }

    pub fn get_global_value(self: *Module, name: String) ?Value {
        for (self.globals.items) |global| {
            if (global.name == name) return global.value;
        }
        return null;
    }

    pub fn get_global_index_name(self: *Module, index: usize) ?[]const u8 {
        const global = self.globals.items[index];
        return self.moon.get_string(global.name);
    }

    pub fn generate_module_function(
        self: *Module,
        tree: *const Moon_AST,
        node_index: NodeIndex,
    ) MoonErrors!*Function {
        const func = try self.create_function();
        const index = node_index.as_usize();
        const node = tree.nodes.items[index];
        if (self.trace) {
            std.debug.print("generate_module_code {} {s}\n", .{
                node_index,
                @tagName(node),
            });
        }

        switch (node) {
            .block => |stmts| {
                for (stmts.items) |stmt| {
                    try self.generate_module_code_block(func, tree, stmt);
                }
            },
            else => {
                return error.BlockExpected;
            },
        }
        return func;
    }

    pub fn generate_function(
        self: *Module,
        func: *Function,
        tree: *const Moon_AST,
    ) MoonErrors!void {
        const node_index = func.block;
        const index = node_index.as_usize();
        const node = tree.nodes.items[index];
        if (self.trace) {
            std.debug.print("generate_function {} {s}\n", .{
                node_index,
                @tagName(node),
            });
        }
        switch (node) {
            .block => |stmts| {
                for (stmts.items) |stmt| {
                    try func.generate_code_block(tree, stmt);
                }
            },
            else => {
                return error.BlockExpected;
            },
        }
        if (func.code.items.len > 0) {
            const last_instr = func.code.items[func.code.items.len - 1];
            if (last_instr.len() == .long) {
                const long_instr = last_instr.get_long();
                if (long_instr.op == .ret) {
                    return;
                }
            }
        }
        _ = try func.add_code(.nil, 0);
        _ = try func.add_code(.ret, 0);
    }

    pub fn generate_module_code_block(
        self: *Module,
        func: *Function,
        tree: *const Moon_AST,
        node_index: NodeIndex,
    ) MoonErrors!void {
        const index = node_index.as_usize();
        const node = tree.nodes.items[index];
        if (self.trace) {
            std.debug.print("generate_module_code_block {} {s}\n", .{
                node_index,
                @tagName(node),
            });
        }
        switch (node) {
            .op_add,
            .op_sub,
            .op_mul,
            .op_div,
            .op_mod,
            .op_lsh,
            .op_rsh,
            .op_band,
            .op_bor,
            .op_bxor,
            .op_lt,
            .op_lte,
            .op_gt,
            .op_gte,
            .op_eq,
            .op_neq,
            .op_and,
            .op_or,
            .call,
            => {
                try func.generate_code(tree, node_index, .{});
            },
            .const_decl => |decl| {
                try func.generate_code(tree, decl.expr, .{});
                const name = try self.moon.intern_string(decl.name);
                if (self.find_global(name)) |i| {
                    const arg: LongArg = @truncate(i);
                    _ = try func.add_code(.store_global, arg);
                } else {
                    return error.UnknownVariable;
                }
            },
            .var_decl => |decl| {
                try func.generate_code(tree, decl.expr, .{});
                const name = try self.moon.intern_string(decl.name);
                if (self.find_global(name)) |i| {
                    const arg: LongArg = @truncate(i);
                    _ = try func.add_code(.store_global, arg);
                } else {
                    return error.UnknownVariable;
                }
            },
            else => {
                try func.generate_code(tree, node_index, .{});
            },
        }
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const CodeAddr = enum(u24) { _ };

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Function = struct {
    module: *Module,
    code: std.ArrayListUnmanaged(Instruction) = .empty,
    constants: std.ArrayListUnmanaged(Value) = .empty,
    locals: std.ArrayListUnmanaged(Local) = .empty,
    num_params: u8 = 0,
    block: NodeIndex = .none,
    need_generation: bool = true,
    trace: bool = false,

    fn deinit(self: *Function) void {
        self.code.deinit(self.module.moon.allocator);
        self.constants.deinit(self.module.moon.allocator);
        self.locals.deinit(self.module.moon.allocator);
    }

    const GenerateCodeOptions = struct {
        assignment: bool = false,
    };

    pub fn generate_code(
        self: *Function,
        tree: *const Moon_AST,
        node_index: NodeIndex,
        options: GenerateCodeOptions,
    ) MoonErrors!void {
        const index = node_index.as_usize();
        const node = tree.nodes.items[index];
        if (self.module.trace) {
            std.debug.print("generate_code {} {s}\n", .{ node_index, @tagName(node) });
        }
        // first generate the children nodes
        switch (node) {
            .op_add,
            .op_sub,
            .op_mul,
            .op_div,
            .op_mod,
            .op_lsh,
            .op_rsh,
            .op_band,
            .op_bor,
            .op_bxor,
            .op_lt,
            .op_lte,
            .op_gt,
            .op_gte,
            .op_eq,
            .op_neq,
            .op_and,
            .op_or,
            => |op| {
                try self.generate_code(tree, op.lhs, .{});
                try self.generate_code(tree, op.rhs, .{});
            },
            .op_dot,
            => |op| {
                const lhs_index = op.lhs.as_usize();
                const lhs = tree.nodes.items[lhs_index];
                if (lhs == .identifier) {
                    try self.generate_code(tree, op.lhs, .{});
                } else {
                    try self.generate_code(tree, op.lhs, .{});
                }
                const rhs_index = op.rhs.as_usize();
                const rhs = tree.nodes.items[rhs_index];

                const i = try self.add_constant_string(rhs.identifier);
                const arg: LongArg = @truncate(i);
                if (options.assignment) {
                    _ = try self.add_code(.store_table_k, arg);
                } else {
                    _ = try self.add_code(.load_table_k, arg);
                }
            },
            else => {},
        }
        // then generate the instruction
        switch (node) {
            .nil => _ = try self.add_code(.nil, 0),
            .boolean_true => _ = try self.add_code(.boolean_true, 0),
            .boolean_false => _ = try self.add_code(.boolean_false, 0),
            .integer_literal => |i| {
                if (i >= std.math.minInt(LongIArg) and i <= std.math.maxInt(LongIArg)) {
                    const value: LongArg = @bitCast(@as(LongIArg, @truncate(i)));
                    _ = try self.add_code(.integer, value);
                } else {
                    const ci = try self.add_constant_i64(i);
                    _ = try self.add_short_code(.constant, ci);
                }
            },
            .number_literal => |n| {
                const ci = try self.add_constant_f64(n);
                _ = try self.add_short_code(.constant, ci);
            },
            .string => |str| {
                if (string_is_simple(str)) {
                    const i = try self.add_constant_string(str);
                    if (i <= std.math.maxInt(ShortArg)) {
                        _ = try self.add_short_code(.constant, @truncate(i));
                    } else {
                        return error.TooManyConstants;
                    }
                } else {
                    var buffer = std.ArrayList(u8).init(self.module.moon.allocator);
                    defer buffer.deinit();
                    const writer = buffer.writer();
                    try expand_string(str, writer);
                    const i = try self.add_constant_string(buffer.items);
                    if (i <= std.math.maxInt(ShortArg)) {
                        _ = try self.add_short_code(.constant, @truncate(i));
                    } else {
                        return error.TooManyConstants;
                    }
                }
            },
            .op_add => _ = try self.add_code(.add, 0),
            .op_sub => _ = try self.add_code(.sub, 0),
            .op_mul => _ = try self.add_code(.mul, 0),
            .op_div => _ = try self.add_code(.div, 0),
            .op_mod => _ = try self.add_code(.mod, 0),
            .op_lsh => _ = try self.add_code(.lsh, 0),
            .op_rsh => _ = try self.add_code(.rsh, 0),
            .op_band => _ = try self.add_code(.band, 0),
            .op_bor => _ = try self.add_code(.bor, 0),
            .op_bxor => _ = try self.add_code(.bxor, 0),
            .op_lt => _ = try self.add_code(.lt, 0),
            .op_gt => _ = try self.add_code(.gt, 0),
            .op_lte => _ = try self.add_code(.lte, 0),
            .op_gte => _ = try self.add_code(.gte, 0),
            .op_eq => _ = try self.add_code(.eq, 0),
            .op_neq => _ = try self.add_code(.neq, 0),
            .op_and => _ = try self.add_code(.@"and", 0),
            .op_or => _ = try self.add_code(.@"or", 0),
            .op_dot => {},
            .block => |stmts| {
                for (stmts.items) |stmt| {
                    try self.generate_code_block(tree, stmt);
                }
            },
            .while_stmt => |stmt| {
                const first_jump = try self.add_short_code(.jmp, 0);
                const start_block = self.get_code_addr();
                try self.generate_code_block(tree, stmt.block);
                const end_block = self.get_code_addr();
                try self.generate_code(tree, stmt.expr, .{});
                try self.add_bra(start_block);
                try self.patch_jmp(first_jump, end_block);
            },
            .call => |stmt| {
                if (try tree.is_builtin(stmt.func, stmt.args.len)) {
                    for (stmt.args) |arg| {
                        try self.generate_code(tree, arg, .{});
                    }
                    try self.generate_call_builtin(tree, stmt.func);
                } else {
                    try self.generate_code(tree, stmt.func, .{});
                    for (stmt.args) |arg| {
                        try self.generate_code(tree, arg, .{});
                    }
                    const num_args: LongArg = @truncate(stmt.args.len);
                    _ = try self.add_code(.call, num_args);
                }
            },
            .return_stmt => |stmt| {
                try self.generate_code(tree, stmt, .{});
                _ = try self.add_code(.ret, 0);
            },
            .identifier => |str| {
                const name = try self.module.moon.intern_string(str);
                if (self.module.find_global(name)) |i| {
                    const arg: LongArg = @truncate(i);
                    if (options.assignment) {
                        _ = try self.add_code(.store_global, arg);
                    } else {
                        _ = try self.add_code(.load_global, arg);
                    }
                } else if (self.find_local_variable(name)) |i| {
                    const arg: LongArg = @truncate(i);
                    if (options.assignment) {
                        _ = try self.add_code(.store_local, arg);
                    } else {
                        _ = try self.add_code(.load_local, arg);
                    }
                } else if (self.find_local_constant(name)) |i| {
                    const arg: LongArg = @truncate(i);
                    if (options.assignment) {
                        std.debug.print("{s} constant\n", .{str});
                        return error.NotVariable;
                    } else {
                        _ = try self.add_code(.load_local, arg);
                    }
                } else if (std.mem.eql(u8, "_", str)) {
                    _ = try self.add_code(.drop, 1);
                } else {
                    std.debug.print("{s} unknown\n", .{str});
                    return error.UnknownVariable;
                }
            },
            .const_decl,
            .var_decl,
            => |decl| {
                try self.generate_code(tree, decl.expr, .{});
                const name = try self.module.moon.intern_string(decl.name);
                if (self.module.find_global(name)) |i| {
                    const arg: LongArg = @truncate(i);
                    _ = try self.add_code(.store_global, arg);
                } else if (self.find_local(name)) |i| {
                    const arg: LongArg = @truncate(i);
                    _ = try self.add_code(.store_local, arg);
                } else {
                    return error.UnknownVariable;
                }
            },
            .assignment => |stmt| {
                try self.generate_code(tree, stmt.rhs, .{});
                try self.generate_code(tree, stmt.lhs, .{ .assignment = true });
            },
            .at_import => {},
            .func_decl => {},
            .table_decl => |decl| {
                _ = try self.add_code(.new_table, 0);
                for (decl) |entry| {
                    if (entry.key == null) {
                        try self.generate_code(tree, entry.value, .{});
                        _ = try self.add_code(.append_table, 0);
                    }
                }
            },
            else => {
                std.debug.print("generate_code: {s}\n", .{@tagName(node)});
                return error.NotImplemented;
            },
        }
    }

    pub fn generate_code_block(
        self: *Function,
        tree: *const Moon_AST,
        node_index: NodeIndex,
    ) MoonErrors!void {
        const index = node_index.as_usize();
        const node = tree.nodes.items[index];
        if (self.trace) {
            std.debug.print("generate_code_block {} {s}\n", .{
                node_index,
                @tagName(node),
            });
        }
        switch (node) {
            .op_add,
            .op_sub,
            .op_mul,
            .op_div,
            .op_mod,
            .op_lsh,
            .op_rsh,
            .op_band,
            .op_bor,
            .op_bxor,
            .op_lt,
            .op_lte,
            .op_gt,
            .op_gte,
            .op_eq,
            .op_neq,
            .op_and,
            .op_or,
            .call,
            => {
                try self.generate_code(tree, node_index, .{});
                _ = try self.add_code(.drop, 1);
            },
            .const_decl => |decl| {
                const name = try self.module.moon.intern_string(decl.name);
                try self.declare_local(name, .constant);
                try self.generate_code(tree, decl.expr, .{});
                if (self.module.find_global(name)) |i| {
                    const arg: LongArg = @truncate(i);
                    _ = try self.add_code(.store_global, arg);
                } else if (self.find_local(name)) |i| {
                    const arg: LongArg = @truncate(i);
                    _ = try self.add_code(.store_local, arg);
                } else {
                    return error.UnknownVariable;
                }
            },
            .var_decl => |decl| {
                const name = try self.module.moon.intern_string(decl.name);
                try self.declare_local(name, .variable);
                try self.generate_code(tree, decl.expr, .{});
                if (self.module.find_global(name)) |i| {
                    const arg: LongArg = @truncate(i);
                    _ = try self.add_code(.store_global, arg);
                } else if (self.find_local(name)) |i| {
                    const arg: LongArg = @truncate(i);
                    _ = try self.add_code(.store_local, arg);
                } else {
                    return error.UnknownVariable;
                }
            },
            else => {
                try self.generate_code(tree, node_index, .{});
            },
        }
    }

    pub fn generate_call_builtin(
        self: *Function,
        tree: *const Moon_AST,
        builtin: NodeIndex,
    ) MoonErrors!void {
        const index = builtin.as_usize();
        const node = tree.nodes.items[index];
        switch (node) {
            .at_import => {
                _ = try self.add_code(.import, 0);
            },
            else => {
                return error.InvalidBuiltin;
            },
        }
    }

    pub fn declare_local(self: *Function, name: String, kind: LocalKind) !void {
        if (self.module.find_global(name)) |_| {
            std.debug.print("{s} already declared (declare_local / global)\n", .{
                self.module.moon.get_string(name),
            });
            return error.NameAlreadyDeclared;
        }
        if (self.find_local(name)) |_| {
            std.debug.print("{s} already declared (declare_local / local)\n", .{
                self.module.moon.get_string(name),
            });
            return error.NameAlreadyDeclared;
        }

        try self.locals.append(self.module.moon.allocator, .{
            .name = name,
            .kind = kind,
        });
    }

    pub fn find_local(self: *Function, name: String) ?u32 {
        for (self.locals.items, 0..) |local, index| {
            if (local.name == name) {
                return @truncate(index);
            }
        }
        return null;
    }

    pub fn find_local_variable(self: *Function, name: String) ?u32 {
        for (self.locals.items, 0..) |local, index| {
            if (local.name == name) {
                if (local.kind == .variable) {
                    return @truncate(index);
                }
            }
        }
        return null;
    }

    pub fn find_local_constant(self: *Function, name: String) ?u32 {
        for (self.locals.items, 0..) |local, index| {
            if (local.name == name) {
                if (local.kind == .constant) {
                    return @truncate(index);
                }
            }
        }
        return null;
    }

    pub fn add_constant_i64(self: *Function, value: i64) MoonErrors!LongArg {
        for (self.constants.items, 0..) |item, i| {
            if (item == .integer and item.integer == value) {
                return @intCast(i);
            }
        }
        const index: LongArg = @intCast(self.constants.items.len);
        try self.constants.append(self.module.moon.allocator, .{
            .literal_integer = value,
        });
        return index;
    }

    pub fn add_constant_f64(self: *Function, value: f64) MoonErrors!LongArg {
        for (self.constants.items, 0..) |item, i| {
            if (item == .number and item.number == value) {
                return @intCast(i);
            }
        }
        const index: LongArg = @intCast(self.constants.items.len);
        try self.constants.append(self.module.moon.allocator, .{
            .literal_number = value,
        });
        return index;
    }

    pub fn add_constant_string(self: *Function, value: []const u8) MoonErrors!u32 {
        const str = try self.module.moon.intern_string(value);
        const index: u32 = @intCast(self.constants.items.len);
        try self.constants.append(self.module.moon.allocator, .{
            .string = str,
        });
        return index;
    }

    pub fn get_code_addr(self: *Function) CodeAddr {
        return @enumFromInt(self.code.items.len);
    }

    pub fn add_code(self: *Function, op: LongOpcode, arg: LongArg) MoonErrors!CodeAddr {
        const index: u24 = @intCast(self.code.items.len);
        try self.code.append(self.module.moon.allocator, Instruction.long(op, arg));
        return @enumFromInt(index);
    }

    pub fn add_short_code(
        self: *Function,
        op: ShortOpcode,
        arg: ShortArg,
    ) MoonErrors!CodeAddr {
        const index: u24 = @intCast(self.code.items.len);
        try self.code.append(self.module.moon.allocator, Instruction.short(op, arg));
        return @enumFromInt(index);
    }

    pub fn add_jmp(self: *Function, offset_addr: CodeAddr) MoonErrors!void {
        const index: i64 = @intCast(self.code.items.len);
        const offset: i64 = @intFromEnum(offset_addr);
        const diff = offset - index;
        if (diff >= std.math.minInt(ShortIArg) and diff <= std.math.maxInt(ShortIArg)) {
            _ = try self.add_short_code(.jmp, @bitCast(@as(ShortIArg, @truncate(diff))));
        } else {
            const kindex = try self.add_constant_i64(diff);
            _ = try self.add_short_code(.jmpk, kindex);
        }
    }

    pub fn add_bra(self: *Function, offset_addr: CodeAddr) MoonErrors!void {
        const index: i64 = @intCast(self.code.items.len);
        const offset: i64 = @intFromEnum(offset_addr);
        const diff = offset - index;
        if (diff >= std.math.minInt(ShortIArg) and diff <= std.math.maxInt(ShortIArg)) {
            _ = try self.add_short_code(.bra, @bitCast(@as(ShortIArg, @truncate(diff))));
        } else {
            const kindex = try self.add_constant_i64(diff);
            _ = try self.add_short_code(.brak, kindex);
        }
    }

    pub fn patch_jmp(
        self: *Function,
        code_addr: CodeAddr,
        target_addr: CodeAddr,
    ) MoonErrors!void {
        const code: i64 = @intFromEnum(code_addr);
        const offset: i64 = @intFromEnum(target_addr);
        const diff = offset - code;
        const addr: usize = @bitCast(code);
        if (diff >= std.math.minInt(ShortIArg) and diff <= std.math.maxInt(ShortIArg)) {
            const arg: ShortArg = @bitCast(@as(ShortIArg, @truncate(diff)));
            self.code.items[addr] = Instruction.short(.jmp, arg);
        } else {
            const kindex = try self.add_constant_i64(diff);
            const arg: ShortArg = @intCast(kindex);
            self.code.items[addr] = Instruction.short(.jmpk, arg);
        }
    }

    fn dump_code(
        self: *Function,
        indent: usize,
        writer: anytype,
    ) !void {
        for (self.code.items, 0..) |instruction, addr| {
            try self.dump_instruction(addr, instruction, indent, writer);
            try writer.print("\n", .{});
        }
    }

    fn dump_instruction(
        self: *Function,
        addr: usize,
        instruction: Instruction,
        indent: usize,
        writer: anytype,
    ) !void {
        try writer.print("{s}", .{lots_of_spaces[0 .. indent * 2]});
        switch (instruction.len()) {
            .short => {
                const code = instruction.get_short();
                switch (code.op) {
                    .constant => {
                        const constant = self.constants.items[code.arg];
                        switch (constant) {
                            .string => |s| {
                                const name = self.module.moon.get_string(s);
                                try writer.print("{x:0>6} {s} constants.{} ; \"{}\"", .{
                                    addr,
                                    @tagName(code.op),
                                    code.arg,
                                    std.zig.fmtEscapes(name),
                                });
                            },
                            else => {
                                try writer.print("{x:0>6} {s} constants.{} ; {s}", .{
                                    addr,
                                    @tagName(code.op),
                                    code.arg,
                                    @tagName(constant),
                                });
                            },
                        }
                    },
                    .jmpk,
                    .brak,
                    => {
                        const name = @tagName(code.op);
                        try writer.print("{x:0>6} {s} {}", .{
                            addr,
                            name,
                            code.arg,
                        });
                    },
                    .jmp,
                    .bra,
                    => {
                        const name = @tagName(code.op);
                        const iarg: ShortIArg = @bitCast(code.arg);
                        try writer.print("{x:0>6} {s} {}", .{ addr, name, iarg });
                    },
                }
            },
            .long => {
                const code = instruction.get_long();
                switch (code.op) {
                    .integer,
                    .load_local,
                    .store_local,
                    .call,
                    .drop,
                    => {
                        try writer.print("{x:0>6} {s} {}", .{
                            addr,
                            @tagName(code.op),
                            code.arg,
                        });
                    },
                    .load_table_k,
                    .store_table_k,
                    => {
                        const constant = self.constants.items[code.arg];
                        switch (constant) {
                            .string => |s| {
                                const name = self.module.moon.get_string(s);
                                try writer.print("{x:0>6} {s} constants.{} ; \"{}\"", .{
                                    addr,
                                    @tagName(code.op),
                                    code.arg,
                                    std.zig.fmtEscapes(name),
                                });
                            },
                            else => {
                                try writer.print("{x:0>6} {s} constants.{} ; {s}", .{
                                    addr,
                                    @tagName(code.op),
                                    code.arg,
                                    @tagName(constant),
                                });
                            },
                        }
                    },
                    .load_global,
                    .store_global,
                    => {
                        if (self.module.get_global_index_name(code.arg)) |name| {
                            try writer.print("{x:0>6} {s} globals.{} ; \"{}\"", .{
                                addr,
                                @tagName(code.op),
                                code.arg,
                                std.zig.fmtEscapes(name),
                            });
                        } else {
                            try writer.print("{x:0>6} {s} globals.{}", .{
                                addr,
                                @tagName(code.op),
                                code.arg,
                            });
                        }
                    },
                    .nop,
                    .nil,
                    .boolean_true,
                    .boolean_false,
                    .import,
                    .add,
                    .sub,
                    .mul,
                    .div,
                    .mod,
                    .lsh,
                    .rsh,
                    .band,
                    .bor,
                    .bxor,
                    .lt,
                    .gt,
                    .lte,
                    .gte,
                    .eq,
                    .neq,
                    .@"and",
                    .@"or",
                    .ret,
                    .new_table,
                    .append_table,
                    => {
                        try writer.print("{x:0>6} {s}", .{
                            addr,
                            @tagName(code.op),
                        });
                    },
                }
            },
        }
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const NodeIndex = enum(u32) {
    none = 0,

    _,

    pub fn as_usize(self: NodeIndex) usize {
        return @intFromEnum(self);
    }

    pub fn format(self: NodeIndex, _: anytype, _: anytype, writer: anytype) !void {
        try writer.print("N#{}", .{self.as_usize()});
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Moon_AST = struct {
    moon: *Moon,
    source: []const u8,
    nodes: std.ArrayListUnmanaged(AST_Node) = .empty,
    trace: bool = false,

    pub fn deinit(self: *Moon_AST) void {
        for (self.nodes.items) |*node| {
            switch (node.*) {
                .block => |*stmts| {
                    self.moon.allocator.free(stmts.items);
                },
                .call => |*call| {
                    self.moon.allocator.free(call.args);
                },
                .if_stmt => |*if_stmt| {
                    self.moon.allocator.free(if_stmt.conds);
                },
                .func_decl => |*func| {
                    self.moon.allocator.free(func.params);
                },
                .table_decl => |table| {
                    self.moon.allocator.free(table);
                },
                else => {},
            }
        }
        self.nodes.deinit(self.moon.allocator);
    }

    pub fn parse(self: *Moon_AST, iter: *TokenIterator) MoonErrors!NodeIndex {
        return try self.parse_statements(iter);
    }

    pub fn parse_statements(self: *Moon_AST, iter: *TokenIterator) MoonErrors!NodeIndex {
        var stmts: std.ArrayListUnmanaged(NodeIndex) = .empty;
        errdefer stmts.deinit(self.moon.allocator);

        iter.skip_whitespace();
        while (iter.peek()) |tk| {
            if (self.trace) {
                std.debug.print("parse_statements: {?}\n", .{tk});
            }

            if (is_expression(tk.kind)) {
                var lhs = try self.parse_expr(iter);
                if (iter.peek()) |atk| {
                    if (atk.kind == .op_assign) {
                        _ = iter.next();
                        const rhs = try self.parse_expr(iter);
                        lhs = try self.new_node(
                            .{ .assignment = .{ .lhs = lhs, .rhs = rhs } },
                        );
                    }
                }
                try stmts.append(self.moon.allocator, lhs);
            } else {
                switch (tk.kind) {
                    .keyword_var => {
                        _ = iter.next();
                        const decl = try self.parse_var_decl(iter);
                        try stmts.append(self.moon.allocator, decl);
                    },
                    .keyword_const => {
                        _ = iter.next();
                        const decl = try self.parse_const_decl(iter);
                        try stmts.append(self.moon.allocator, decl);
                    },
                    .keyword_while => {
                        _ = iter.next();
                        const stmt = try self.parse_while(iter);
                        try stmts.append(self.moon.allocator, stmt);
                    },
                    .keyword_if => {
                        _ = iter.next();
                        const stmt = try self.parse_if(iter);
                        try stmts.append(self.moon.allocator, stmt);
                    },
                    .keyword_fn => {
                        _ = iter.next();
                        const stmt = try self.parse_function_decl(iter);
                        try stmts.append(self.moon.allocator, stmt);
                    },
                    .keyword_return => {
                        _ = iter.next();
                        if (iter.peek()) |ntk| {
                            if (is_expression(ntk.kind)) {
                                const expr = try self.parse_expr(iter);
                                const stmt = try self.new_node(
                                    .{ .return_stmt = expr },
                                );
                                try stmts.append(self.moon.allocator, stmt);
                            } else {
                                const expr = try self.new_node(.nil);
                                const stmt = try self.new_node(
                                    .{ .return_stmt = expr },
                                );
                                try stmts.append(self.moon.allocator, stmt);
                            }
                        } else {
                            const expr = try self.new_node(.nil);
                            const stmt = try self.new_node(
                                .{ .return_stmt = expr },
                            );
                            try stmts.append(self.moon.allocator, stmt);
                        }
                    },
                    .eos => {
                        _ = iter.next();
                    },
                    .open_block => {
                        _ = iter.next();
                        const block = try self.parse_statements(iter);

                        if (iter.peek()) |etk| {
                            if (self.trace) {
                                std.debug.print("parse_block: {?}\n", .{etk});
                            }
                            if (etk.kind != .close_block) {
                                std.debug.print("Missing Close Block {}\n", .{tk});
                                return error.MissingCloseBlock;
                            }
                            _ = iter.next();
                        }
                        try stmts.append(self.moon.allocator, block);
                    },
                    .close_block => {
                        break;
                    },
                    else => {
                        std.debug.print("Invalid statement: {}\n", .{tk});
                        return error.InvalidStatement;
                    },
                }
            }
            iter.skip_whitespace();
        }

        return try self.new_node(.{ .block = .{
            .items = try stmts.toOwnedSlice(self.moon.allocator),
        } });
    }

    pub fn parse_var_decl(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        if (iter.peek()) |tk| {
            if (self.trace) {
                std.debug.print("parse_var_decl: {?}\n", .{tk});
            }

            if (tk.kind != .identifier) {
                return error.MissingIdentifier;
            }

            const name = tk.get_string(self.source);

            if (iter.next()) |ntk| {
                if (ntk.kind != .op_assign) {
                    return error.MissingAssignment;
                }

                _ = iter.next();
            } else {
                return error.MissingAssignment;
            }

            const expr = try self.parse_expr(iter);

            return try self.new_node(
                .{ .var_decl = .{ .name = name, .expr = expr } },
            );
        }
        return error.InvalidVariableDeclaration;
    }

    pub fn parse_const_decl(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        if (iter.peek()) |tk| {
            if (self.trace) {
                std.debug.print("parse_const_decl: {?}\n", .{tk});
            }

            if (tk.kind != .identifier) {
                return error.MissingIdentifier;
            }

            const name = tk.get_string(self.source);

            if (iter.next()) |ntk| {
                if (ntk.kind != .op_assign) {
                    return error.MissingAssignment;
                }

                _ = iter.next();
            } else {
                return error.MissingAssignment;
            }

            const expr = try self.parse_expr(iter);

            return try self.new_node(
                .{ .const_decl = .{ .name = name, .expr = expr } },
            );
        }
        return error.InvalidConstantDeclaration;
    }

    pub fn parse_while(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        if (iter.peek()) |tk| {
            if (self.trace) {
                std.debug.print("parse_while: {?}\n", .{tk});
            }

            const expr = try self.parse_expr(iter);

            const block = try self.parse_block(iter);

            return try self.new_node(
                .{ .while_stmt = .{ .expr = expr, .block = block } },
            );
        }
        return error.InvalidStatement;
    }

    pub fn parse_if(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        var conds: std.ArrayListUnmanaged(IfCond) = .empty;
        errdefer conds.deinit(self.moon.allocator);

        if (iter.peek()) |tk| {
            if (self.trace) {
                std.debug.print("parse_if: {?}\n", .{tk});
            }

            const expr = try self.parse_expr(iter);
            const block = try self.parse_block(iter);

            try conds.append(
                self.moon.allocator,
                .{ .expr = expr, .block = block },
            );

            iter.skip_whitespace();

            while (iter.peek()) |etk| {
                if (self.trace) {
                    std.debug.print("parse_if: {?}\n", .{tk});
                }

                if (etk.kind == .keyword_elif) {
                    _ = iter.next();
                    const elif_expr = try self.parse_expr(iter);
                    const elif_block = try self.parse_block(iter);

                    try conds.append(
                        self.moon.allocator,
                        .{ .expr = elif_expr, .block = elif_block },
                    );
                } else {
                    break;
                }

                iter.skip_whitespace();
            }

            var else_block: ?NodeIndex = null;

            if (iter.peek()) |etk| {
                if (etk.kind == .keyword_else) {
                    _ = iter.next();
                    else_block = try self.parse_block(iter);
                }
            }

            return try self.new_node(
                .{ .if_stmt = .{
                    .conds = try conds.toOwnedSlice(self.moon.allocator),
                    .else_block = else_block,
                } },
            );
        }
        return error.InvalidStatement;
    }

    pub fn parse_function_decl(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        iter.skip_whitespace();
        if (iter.peek()) |tk| {
            if (self.trace) {
                std.debug.print("parse_function_decl: {?}\n", .{tk});
            }
            if (tk.kind == .identifier) {
                _ = iter.next();
                const name = tk.get_string(self.source);
                const params = try self.parse_parameter_list(iter);
                const block = try self.parse_block(iter);

                return try self.new_node(
                    .{ .func_decl = .{
                        .name = name,
                        .params = params,
                        .block = block,
                    } },
                );
            } else {
                return error.MissingIdentifier;
            }
        }
        return error.InvalidFunctionDeclaration;
    }

    pub fn parse_parameter_list(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors![]Parameter {
        var params: std.ArrayListUnmanaged(Parameter) = .empty;
        errdefer params.deinit(self.moon.allocator);

        if (iter.peek()) |tk| {
            if (self.trace) {
                std.debug.print("parse_parameter_list: {?}\n", .{tk});
            }
            if (tk.kind != .open_round) {
                std.debug.print("Missing Open Round {}\n", .{tk});
                return error.MissingOpenParenthesis;
            }

            _ = iter.next();

            while (iter.peek()) |ntk| {
                if (ntk.kind == .identifier) {
                    _ = iter.next();
                    try params.append(self.moon.allocator, .{
                        .name = ntk.get_string(self.source),
                    });
                } else {
                    break;
                }

                iter.skip_whitespace();

                if (iter.peek()) |ctk| {
                    if (ctk.kind == .comma) {
                        _ = iter.next();
                    }
                }

                iter.skip_whitespace();
            }

            if (iter.peek()) |etk| {
                if (etk.kind == .close_round) {
                    _ = iter.next();

                    return params.toOwnedSlice(self.moon.allocator);
                } else {
                    return error.MissingCloseParenthesis;
                }
            }
        }
        return error.InvalidParameterList;
    }

    pub fn parse_block(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        iter.skip_whitespace();
        if (iter.peek()) |tk| {
            if (self.trace) {
                std.debug.print("parse_block: {?}\n", .{tk});
            }
            if (tk.kind != .open_block) {
                std.debug.print("Missing Open Block {}\n", .{tk});
                return error.MissingOpenBlock;
            }

            _ = iter.next();
            const stmts = try self.parse_statements(iter);

            if (iter.peek()) |etk| {
                if (self.trace) {
                    std.debug.print("parse_block: {?}\n", .{etk});
                }
                if (etk.kind != .close_block) {
                    std.debug.print("Missing Close Block {}\n", .{tk});
                    return error.MissingCloseBlock;
                }
                _ = iter.next();
            }

            return stmts;
        }
        return error.NotImplemented;
    }

    pub fn parse_expr(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        if (self.trace) {
            const tk = iter.peek();
            std.debug.print("parse_expr: {?}\n", .{tk});
        }

        var lhs = try self.parse_logical_and_expr(iter);
        while (iter.peek()) |op| {
            if (self.trace) {
                std.debug.print("parse_expr: {}\n", .{op});
            }
            if (op.kind == .keyword_or) {
                _ = iter.next();
                const rhs = try self.parse_logical_and_expr(iter);
                lhs = try self.new_node(
                    .{ .op_or = .{ .lhs = lhs, .rhs = rhs } },
                );
            } else {
                break;
            }
        }
        return lhs;
    }

    pub fn parse_logical_and_expr(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        if (self.trace) {
            const tk = iter.peek();
            std.debug.print("parse_logical_and_expr: {?}\n", .{tk});
        }

        var lhs = try self.parse_compare_expr(iter);
        while (iter.peek()) |op| {
            if (self.trace) {
                std.debug.print("parse_logical_and_expr: {}\n", .{op});
            }
            if (op.kind == .keyword_and) {
                _ = iter.next();
                const rhs = try self.parse_compare_expr(iter);
                lhs = try self.new_node(
                    .{ .op_and = .{ .lhs = lhs, .rhs = rhs } },
                );
            } else {
                break;
            }
        }
        return lhs;
    }

    pub fn parse_compare_expr(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        if (self.trace) {
            const tk = iter.peek();
            std.debug.print("parse_compare_expr: {?}\n", .{tk});
        }
        var lhs = try self.parse_bitwise_expr(iter);
        if (iter.peek()) |op| {
            if (self.trace) {
                std.debug.print("parse_compae_expr: {}\n", .{op});
            }
            switch (op.kind) {
                .op_lt => {
                    _ = iter.next();
                    const rhs = try self.parse_bitwise_expr(iter);
                    lhs = try self.new_node(
                        .{ .op_lt = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_lte => {
                    _ = iter.next();
                    const rhs = try self.parse_bitwise_expr(iter);
                    lhs = try self.new_node(
                        .{ .op_lte = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_gt => {
                    _ = iter.next();
                    const rhs = try self.parse_bitwise_expr(iter);
                    lhs = try self.new_node(
                        .{ .op_gt = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_gte => {
                    _ = iter.next();
                    const rhs = try self.parse_bitwise_expr(iter);
                    lhs = try self.new_node(
                        .{ .op_gte = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_neq => {
                    _ = iter.next();
                    const rhs = try self.parse_bitwise_expr(iter);
                    lhs = try self.new_node(
                        .{ .op_neq = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_eq => {
                    _ = iter.next();
                    const rhs = try self.parse_bitwise_expr(iter);
                    lhs = try self.new_node(
                        .{ .op_eq = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                else => {},
            }
        }
        return lhs;
    }

    pub fn parse_bitwise_expr(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        if (self.trace) {
            const tk = iter.peek();
            std.debug.print("parse_bitwise_expr: {?}\n", .{tk});
        }

        var lhs = try self.parse_bitshift_expr(iter);
        while (iter.peek()) |op| {
            if (self.trace) {
                std.debug.print("parse_bitwise_expr: {}\n", .{op});
            }
            switch (op.kind) {
                .op_bor => {
                    _ = iter.next();
                    const rhs = try self.parse_bitshift_expr(iter);
                    lhs = try self.new_node(
                        .{ .op_bor = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_band => {
                    _ = iter.next();
                    const rhs = try self.parse_bitshift_expr(iter);
                    lhs = try self.new_node(
                        .{ .op_band = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_bxor => {
                    _ = iter.next();
                    const rhs = try self.parse_bitshift_expr(iter);
                    lhs = try self.new_node(
                        .{ .op_bxor = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                else => break,
            }
        }
        return lhs;
    }

    pub fn parse_bitshift_expr(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        if (self.trace) {
            const tk = iter.peek();
            std.debug.print("parse_bitshift_expr: {?}\n", .{tk});
        }

        var lhs = try self.parse_addition_expr(iter);
        while (iter.peek()) |op| {
            if (self.trace) {
                std.debug.print("parse_bitshift_expr: {}\n", .{op});
            }
            switch (op.kind) {
                .op_lsh => {
                    _ = iter.next();
                    const rhs = try self.parse_addition_expr(iter);
                    lhs = try self.new_node(
                        .{ .op_lsh = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_rsh => {
                    _ = iter.next();
                    const rhs = try self.parse_addition_expr(iter);
                    lhs = try self.new_node(
                        .{ .op_rsh = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                else => break,
            }
        }
        return lhs;
    }

    pub fn parse_addition_expr(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        if (self.trace) {
            const tk = iter.peek();
            std.debug.print("parse_addition_expr: {?}\n", .{tk});
        }

        var lhs = try self.parse_multiply_expr(iter);
        while (iter.peek()) |op| {
            if (self.trace) {
                std.debug.print("parse_addition_expr: {}\n", .{op});
            }
            switch (op.kind) {
                .op_add => {
                    _ = iter.next();
                    const rhs = try self.parse_multiply_expr(iter);
                    lhs = try self.new_node(
                        .{ .op_add = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_sub => {
                    _ = iter.next();
                    const rhs = try self.parse_multiply_expr(iter);
                    lhs = try self.new_node(
                        .{ .op_sub = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                else => break,
            }
        }
        return lhs;
    }

    pub fn parse_multiply_expr(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        if (self.trace) {
            const tk = iter.peek();
            std.debug.print("parse_multiply_expr: {?}\n", .{tk});
        }

        var lhs = try self.parse_unary_expr(iter);
        while (iter.peek()) |op| {
            if (self.trace) {
                std.debug.print("parse_multiply_expr: {}\n", .{op});
            }
            switch (op.kind) {
                .op_mul => {
                    _ = iter.next();
                    const rhs = try self.parse_unary_expr(iter);
                    lhs = try self.new_node(
                        .{ .op_mul = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_div => {
                    _ = iter.next();
                    const rhs = try self.parse_unary_expr(iter);
                    lhs = try self.new_node(
                        .{ .op_div = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_mod => {
                    _ = iter.next();
                    const rhs = try self.parse_unary_expr(iter);
                    lhs = try self.new_node(
                        .{ .op_mod = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                else => break,
            }
        }
        return lhs;
    }

    pub fn parse_unary_expr(self: *Moon_AST, iter: *TokenIterator) MoonErrors!NodeIndex {
        if (iter.peek()) |tk| {
            if (self.trace) {
                std.debug.print("parse_unary_expr: {}\n", .{tk});
            }

            switch (tk.kind) {
                .keyword_not => {
                    _ = iter.next();
                    const rhs = try self.parse_unary_expr(iter);
                    return try self.new_node(
                        .{ .op_not = rhs },
                    );
                },
                .op_sub => {
                    _ = iter.next();
                    const rhs = try self.parse_unary_expr(iter);
                    return try self.new_node(
                        .{ .op_neg = rhs },
                    );
                },
                .op_com => {
                    _ = iter.next();
                    const rhs = try self.parse_unary_expr(iter);
                    return try self.new_node(
                        .{ .op_com = rhs },
                    );
                },
                else => {},
            }
        }
        return self.parse_atom(iter);
    }

    pub fn parse_atom(self: *Moon_AST, iter: *TokenIterator) MoonErrors!NodeIndex {
        iter.skip_whitespace();
        if (iter.peek()) |tk| {
            if (self.trace) {
                std.debug.print("parse_atom: {}\n", .{tk});
            }

            switch (tk.kind) {
                .keyword_true => {
                    _ = iter.next();
                    return try self.new_node(.boolean_true);
                },
                .keyword_false => {
                    _ = iter.next();
                    return try self.new_node(.boolean_false);
                },
                .keyword_nil => {
                    _ = iter.next();
                    return try self.new_node(.nil);
                },
                .integer => {
                    _ = iter.next();
                    const str = tk.get_string(self.source);
                    return try self.new_node(
                        .{ .integer_literal = try std.fmt.parseInt(i64, str, 0) },
                    );
                },
                .number => {
                    _ = iter.next();
                    const str = tk.get_string(self.source);
                    return try self.new_node(
                        .{ .number_literal = try std.fmt.parseFloat(f64, str) },
                    );
                },
                .open_round => {
                    _ = iter.next();
                    const expr = self.parse_expr(iter);
                    if (iter.peek()) |etk| {
                        if (etk.kind == .close_round) {
                            _ = iter.next();
                            return expr;
                        }
                    }
                    return error.MissingCloseParenthesis;
                },
                .identifier => {
                    return self.parse_prefix(iter);
                },
                .keyword_at_import,
                => {
                    return self.parse_at_prefix(iter);
                },
                .string_literal => {
                    _ = iter.next();
                    return try self.new_node(
                        .{ .string = tk.get_string(self.source) },
                    );
                },
                .dot_block => {
                    _ = iter.next();
                    return self.parse_table_decl(iter);
                },
                else => {
                    std.debug.print("Invalid Expression at {}\n", .{tk});
                    return error.InvalidExpressionAtom;
                },
            }
        }

        return error.InvalidExpression;
    }

    pub fn parse_prefix(self: *Moon_AST, iter: *TokenIterator) MoonErrors!NodeIndex {
        if (iter.peek()) |tk| {
            if (self.trace) {
                std.debug.print("parse_prefix: {}\n", .{tk});
            }
            _ = iter.next();
            var lhs = try self.new_node(
                .{ .identifier = tk.get_string(self.source) },
            );

            while (iter.peek()) |ntk| {
                if (self.trace) {
                    std.debug.print("parse_prefix: {}\n", .{ntk});
                }
                switch (ntk.kind) {
                    .dot => {
                        _ = iter.next();
                        if (iter.peek()) |itk| {
                            if (itk.kind == .identifier) {
                                _ = iter.next();
                                const rhs = try self.new_node(
                                    .{ .identifier = itk.get_string(self.source) },
                                );
                                lhs = try self.new_node(.{ .op_dot = .{
                                    .lhs = lhs,
                                    .rhs = rhs,
                                } });
                            }
                        }
                    },
                    .open_round => {
                        _ = iter.next();
                        const args = try self.parse_args(iter);
                        return self.new_node(.{ .call = .{
                            .func = lhs,
                            .args = args,
                        } });
                    },
                    else => {
                        break;
                    },
                }
            }

            return lhs;
        }
        return error.MissingIdentifier;
    }

    pub fn parse_at_prefix(self: *Moon_AST, iter: *TokenIterator) MoonErrors!NodeIndex {
        if (iter.peek()) |tk| {
            if (self.trace) {
                std.debug.print("parse_prefix: {}\n", .{tk});
            }
            _ = iter.next();
            var lhs: NodeIndex = undefined;
            switch (tk.kind) {
                .keyword_at_import => {
                    lhs = try self.new_node(.at_import);
                },
                else => {
                    return error.InvalidBuiltin;
                },
            }

            if (iter.peek()) |ntk| {
                if (self.trace) {
                    std.debug.print("parse_prefix: {}\n", .{ntk});
                }
                switch (ntk.kind) {
                    .open_round => {
                        _ = iter.next();
                        const args = try self.parse_args(iter);
                        return self.new_node(.{ .call = .{
                            .func = lhs,
                            .args = args,
                        } });
                    },
                    else => {
                        return error.MissingOpenParenthesis;
                    },
                }
            }

            return lhs;
        }
        return error.MissingIdentifier;
    }

    pub fn parse_table_decl(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        var entries: std.ArrayListUnmanaged(TableEntry) = .empty;
        errdefer entries.deinit(self.moon.allocator);

        iter.skip_whitespace();
        while (iter.peek()) |tk| {
            if (self.trace) {
                std.debug.print("parse_table_decl: {}\n", .{tk});
            }

            if (tk.kind == .dot) {
                _ = iter.next();
                if (iter.peek()) |ctk| {
                    if (self.trace) {
                        std.debug.print("parse_table_decl: {}\n", .{ctk});
                    }
                    if (ctk.kind == .identifier) {
                        _ = iter.next();
                        const first = try self.new_node(.{
                            .identifier = ctk.get_string(self.source),
                        });

                        iter.skip_whitespace();

                        if (iter.peek()) |atk| {
                            if (self.trace) {
                                std.debug.print("parse_table_decl: {}\n", .{atk});
                            }
                            if (atk.kind == .op_assign) {
                                _ = iter.next();

                                const second = try self.parse_expr(iter);

                                try entries.append(self.moon.allocator, .{
                                    .key = first,
                                    .value = second,
                                });
                            } else {
                                std.debug.print("Missing Assignment at {}\n", .{tk});
                                return error.MissingAssignment;
                            }
                        }
                    } else {
                        std.debug.print("Missing Identifier at {}\n", .{tk});
                        return error.MissingIdentifier;
                    }
                }
            } else if (is_expression(tk.kind)) {
                const first = try self.parse_expr(iter);
                var second: ?NodeIndex = null;

                iter.skip_whitespace();

                if (iter.peek()) |ctk| {
                    if (self.trace) {
                        std.debug.print("parse_table_decl: {}\n", .{ctk});
                    }
                    if (ctk.kind == .op_assign) {
                        _ = iter.next();

                        second = try self.parse_expr(iter);
                    }
                }

                iter.skip_whitespace();

                if (second) |snd| {
                    try entries.append(self.moon.allocator, .{
                        .key = first,
                        .value = snd,
                    });
                } else {
                    try entries.append(self.moon.allocator, .{
                        .key = null,
                        .value = first,
                    });
                }
            } else {
                if (iter.peek()) |ctk| {
                    if (ctk.kind == .close_block) {
                        _ = iter.next();

                        return self.new_node(.{
                            .table_decl = try entries.toOwnedSlice(self.moon.allocator),
                        });
                    }
                }
                std.debug.print("Invalid Expression at {}\n", .{tk});
                return error.InvalidExpression;
            }

            iter.skip_whitespace();

            if (iter.peek()) |ctk| {
                if (self.trace) {
                    std.debug.print("parse_table_decl: {}\n", .{ctk});
                }
                if (ctk.kind == .comma) {
                    _ = iter.next();
                }
            }

            iter.skip_whitespace();

            if (iter.peek()) |ctk| {
                if (self.trace) {
                    std.debug.print("parse_table_decl: {}\n", .{ctk});
                }
                if (ctk.kind == .close_block) {
                    _ = iter.next();

                    return self.new_node(.{
                        .table_decl = try entries.toOwnedSlice(self.moon.allocator),
                    });
                }
            }
        }
        return error.InvalidExpression;
    }

    pub fn parse_args(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors![]NodeIndex {
        var args: std.ArrayListUnmanaged(NodeIndex) = .empty;
        errdefer args.deinit(self.moon.allocator);

        iter.skip_whitespace();
        while (iter.peek()) |tk| {
            if (self.trace) {
                std.debug.print("parse_args: {}\n", .{tk});
            }
            if (is_expression(tk.kind)) {
                const arg = try self.parse_expr(iter);
                try args.append(self.moon.allocator, arg);

                iter.skip_whitespace();

                if (iter.peek()) |ctk| {
                    if (ctk.kind == .comma) {
                        _ = iter.next();
                    }
                }

                iter.skip_whitespace();

                if (iter.peek()) |ctk| {
                    if (ctk.kind == .close_round) {
                        _ = iter.next();
                        break;
                    }
                }
            } else {
                std.debug.print("Invalid Expression at {}\n", .{tk});
                return error.InvalidExpression;
            }
        }

        return args.toOwnedSlice(self.moon.allocator);
    }

    pub fn is_expression(kind: TokenKind) bool {
        switch (kind) {
            .integer,
            .number,
            .op_neg,
            .open_round,
            .identifier,
            .string_literal,
            .keyword_not,
            .op_sub,
            .op_com,
            .keyword_nil,
            .keyword_true,
            .keyword_false,
            .dot_block,
            => return true,
            else => return false,
        }
    }

    pub fn is_builtin(
        tree: *const Moon_AST,
        node_index: NodeIndex,
        args: usize,
    ) MoonErrors!bool {
        const index = node_index.as_usize();
        const node = tree.nodes.items[index];
        switch (node) {
            .at_import => if (args == 1) return true else return error.InvalidNumberOfArgs,
            else => return false,
        }
    }

    pub fn new_node(self: *Moon_AST, node: AST_Node) MoonErrors!NodeIndex {
        const index = self.nodes.items.len;
        try self.nodes.append(self.moon.allocator, node);
        return @enumFromInt(@as(u32, @intCast(index)));
    }

    const DumpASTOptions = struct {
        show_node_index: bool = false,
    };

    pub fn dump(
        self: *Moon_AST,
        index: NodeIndex,
        options: DumpASTOptions,
        writer: anytype,
    ) !void {
        try writer.print("AST\n", .{});
        try self.dump_internal(index, 1, options, writer);
    }

    fn dump_internal(
        self: *Moon_AST,
        nindex: NodeIndex,
        depth: usize,
        options: DumpASTOptions,
        writer: anytype,
    ) !void {
        const index = nindex.as_usize();
        if (index < self.nodes.items.len) {
            try writer.print("{s}", .{lots_of_spaces[0 .. depth * 2]});
            const node = self.nodes.items[index];
            if (options.show_node_index) {
                try writer.print("{} ", .{nindex});
            }
            try writer.print("{s}", .{@tagName(node)});
            switch (node) {
                .nil,
                .boolean_true,
                .boolean_false,
                => {
                    try writer.print("\n", .{});
                },
                .integer_literal => |i| {
                    try writer.print(" {}\n", .{i});
                },
                .number_literal => |n| {
                    try writer.print(" {d}\n", .{n});
                },
                .identifier => |str| {
                    try writer.print(" {s}\n", .{str});
                },
                .string => |str| {
                    try writer.print(" \"{}\"\n", .{std.zig.fmtEscapes(str)});
                },
                .op_add,
                .op_sub,
                .op_mul,
                .op_div,
                .op_mod,
                .op_lsh,
                .op_rsh,
                .op_band,
                .op_bor,
                .op_bxor,
                .op_lt,
                .op_lte,
                .op_gt,
                .op_gte,
                .op_eq,
                .op_neq,
                .op_and,
                .op_or,
                .op_dot,
                => |op| {
                    try writer.print("\n", .{});
                    try self.dump_internal(op.lhs, depth + 1, options, writer);
                    try self.dump_internal(op.rhs, depth + 1, options, writer);
                },
                .op_not,
                .op_neg,
                .op_com,
                => |op| {
                    try writer.print("\n", .{});
                    try self.dump_internal(op, depth + 1, options, writer);
                },
                .call => |call| {
                    try writer.print("\n", .{});
                    try self.dump_internal(call.func, depth + 1, options, writer);
                    for (call.args) |item| {
                        try writer.print("{s}  arg\n", .{lots_of_spaces[0 .. depth * 2]});
                        try self.dump_internal(item, depth + 2, options, writer);
                    }
                },
                .block => |stmts| {
                    try writer.print("\n", .{});
                    for (stmts.items) |stmt| {
                        try self.dump_internal(stmt, depth + 1, options, writer);
                    }
                },
                .var_decl => |decl| {
                    try writer.print(" {s}\n", .{decl.name});
                    try self.dump_internal(decl.expr, depth + 1, options, writer);
                },
                .const_decl => |decl| {
                    try writer.print(" {s}\n", .{decl.name});
                    try self.dump_internal(decl.expr, depth + 1, options, writer);
                },
                .while_stmt => |stmt| {
                    try writer.print("\n", .{});
                    try self.dump_internal(stmt.expr, depth + 1, options, writer);
                    try self.dump_internal(stmt.block, depth + 1, options, writer);
                },
                .if_stmt => |stmt| {
                    try writer.print("\n", .{});
                    for (stmt.conds) |cond| {
                        try self.dump_internal(cond.expr, depth + 1, options, writer);
                        try self.dump_internal(cond.block, depth + 1, options, writer);
                    }
                    if (stmt.else_block) |block| {
                        try self.dump_internal(block, depth + 1, options, writer);
                    }
                },
                .assignment => |stmt| {
                    try writer.print("\n", .{});
                    try self.dump_internal(stmt.lhs, depth + 1, options, writer);
                    try self.dump_internal(stmt.rhs, depth + 1, options, writer);
                },
                .func_decl => |func| {
                    try writer.print(" {s} (", .{func.name});
                    for (func.params, 0..) |param, pi| {
                        if (pi > 0) {
                            try writer.print(", ", .{});
                        }
                        try writer.print("{s}", .{param.name});
                    }
                    try writer.print(")\n", .{});
                    try self.dump_internal(func.block, depth + 1, options, writer);
                },
                .return_stmt => |stmt| {
                    try writer.print("\n", .{});
                    try self.dump_internal(stmt, depth + 1, options, writer);
                },
                .table_decl => |table| {
                    try writer.print("\n", .{});
                    for (table) |entry| {
                        if (entry.key) |key| {
                            try writer.print("{s}  key\n", .{lots_of_spaces[0 .. depth * 2]});
                            try self.dump_internal(key, depth + 2, options, writer);
                        }
                        try writer.print("{s}  value\n", .{lots_of_spaces[0 .. depth * 2]});
                        try self.dump_internal(entry.value, depth + 2, options, writer);
                    }
                },
                .at_import,
                => {
                    try writer.print("\n", .{});
                },
            }
        }
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const AST_NodeKind = enum {
    nil,
    boolean_true,
    boolean_false,
    integer_literal,
    number_literal,
    identifier,
    string,
    op_add,
    op_sub,
    op_mul,
    op_div,
    op_mod,
    op_lsh,
    op_rsh,
    op_band,
    op_bor,
    op_bxor,
    op_lt,
    op_lte,
    op_gt,
    op_gte,
    op_eq,
    op_neq,
    op_and,
    op_or,
    op_dot,
    op_not,
    op_neg,
    op_com,
    call,
    block,
    var_decl,
    const_decl,
    while_stmt,
    if_stmt,
    assignment,
    func_decl,
    return_stmt,
    table_decl,
    at_import,
};

pub const AST_Node = union(AST_NodeKind) {
    nil,
    boolean_true,
    boolean_false,
    integer_literal: i64,
    number_literal: f64,
    identifier: []const u8,
    string: []const u8,
    op_add: AST_BinaryOp,
    op_sub: AST_BinaryOp,
    op_mul: AST_BinaryOp,
    op_div: AST_BinaryOp,
    op_mod: AST_BinaryOp,
    op_lsh: AST_BinaryOp,
    op_rsh: AST_BinaryOp,
    op_band: AST_BinaryOp,
    op_bor: AST_BinaryOp,
    op_bxor: AST_BinaryOp,
    op_lt: AST_BinaryOp,
    op_lte: AST_BinaryOp,
    op_gt: AST_BinaryOp,
    op_gte: AST_BinaryOp,
    op_eq: AST_BinaryOp,
    op_neq: AST_BinaryOp,
    op_and: AST_BinaryOp,
    op_or: AST_BinaryOp,
    op_dot: AST_BinaryOp,
    op_not: NodeIndex,
    op_neg: NodeIndex,
    op_com: NodeIndex,
    call: Call,
    block: AST_List,
    var_decl: Decl,
    const_decl: Decl,
    while_stmt: WhileStmt,
    if_stmt: IfStmt,
    assignment: AST_BinaryOp,
    func_decl: FunctionDecl,
    return_stmt: NodeIndex,
    table_decl: []TableEntry,
    at_import,
};

pub const AST_BinaryOp = struct {
    lhs: NodeIndex,
    rhs: NodeIndex,
};

pub const AST_List = struct {
    items: []NodeIndex,
};

pub const Call = struct {
    func: NodeIndex,
    args: []NodeIndex,
};

pub const Decl = struct {
    name: []const u8,
    expr: NodeIndex,
};

pub const WhileStmt = struct {
    expr: NodeIndex,
    block: NodeIndex,
};

pub const IfStmt = struct {
    conds: []IfCond,
    else_block: ?NodeIndex,
};

pub const IfCond = struct {
    expr: NodeIndex,
    block: NodeIndex,
};

pub const Parameter = struct {
    name: []const u8,
};

pub const FunctionDecl = struct {
    name: []const u8,
    params: []Parameter,
    block: NodeIndex,
};

pub const TableEntry = struct {
    key: ?NodeIndex,
    value: NodeIndex,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const MoonErrors = error{
    AccessDenied,
    BlockExpected,
    BrokenPipe,
    ConnectionResetByPeer,
    DeviceBusy,
    DiskQuota,
    EndOfFunction,
    ExecuteLimit,
    FileTooBig,
    InputOutput,
    InvalidArgument,
    InvalidBuiltin,
    InvalidCharacter,
    InvalidConstantDeclaration,
    InvalidExpression,
    InvalidExpressionAtom,
    InvalidFunctionDeclaration,
    InvalidFunction,
    InvalidInstruction,
    InvalidNumberOfArgs,
    InvalidParameter,
    InvalidParameterList,
    InvalidStatement,
    InvalidVariableDeclaration,
    LockViolation,
    MessageTooBig,
    ModuleUnknown,
    MissingAssignment,
    MissingCloseBlock,
    MissingCloseParenthesis,
    MissingIdentifier,
    MissingOpenBlock,
    MissingOpenParenthesis,
    MissingStatements,
    NameAlreadyDeclared,
    NoDevice,
    NoSpaceLeft,
    NotImplemented,
    NotInteger,
    NotOpenForWriting,
    NotVariable,
    OperationAborted,
    OutOfMemory,
    Overflow,
    PermissionDenied,
    ProcessNotFound,
    StackUnderflow,
    SystemResources,
    TooManyConstants,
    TypeMismatch,
    TypeError,
    Unexpected,
    UnknownVariable,
    WouldBlock,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const ValueKind = enum {
    nil,
    boolean,
    integer,
    literal_integer,
    number,
    literal_number,
    string,
    function,
    cfunction,
    module,
    table,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Value = union(ValueKind) {
    nil,
    boolean: bool,
    integer: i64,
    literal_integer: i64,
    number: f64,
    literal_number: f64,
    string: String,
    function: FunctionIndex,
    cfunction: CFunction,
    module: ModuleIndex,
    table: TableIndex,

    pub fn is_true(self: Value) bool {
        if (self == .boolean) {
            return self.boolean;
        }
        return false;
    }

    pub fn is_boolean(self: Value) bool {
        switch (self) {
            .boolean => return true,
            else => return false,
        }
    }

    pub fn is_integer(self: Value) bool {
        switch (self) {
            .integer, .literal_integer => return true,
            else => return false,
        }
    }

    pub fn is_number(self: Value) bool {
        switch (self) {
            .number, .literal_number => return true,
            else => return false,
        }
    }

    pub fn is_string(self: Value) bool {
        switch (self) {
            .string => return true,
            else => return false,
        }
    }

    pub fn is_function(self: Value) bool {
        switch (self) {
            .function,
            .cfunction,
            => return true,
            else => return false,
        }
    }

    pub fn is_module(self: Value) bool {
        switch (self) {
            .module => return true,
            else => return false,
        }
    }

    pub fn is_table(self: Value) bool {
        switch (self) {
            .table => return true,
            else => return false,
        }
    }

    pub fn as_string(self: Value) ?String {
        switch (self) {
            .string => |s| return s,
            else => return null,
        }
    }

    pub fn as_module(self: Value) ?ModuleIndex {
        switch (self) {
            .module => |m| return m,
            else => return null,
        }
    }

    pub fn as_table(self: Value) ?TableIndex {
        switch (self) {
            .table => |t| return t,
            else => return null,
        }
    }
};

pub fn value_from_string(string: String) Value {
    return .{ .string = string };
}

pub fn value_from_cfunction(func: CFunction) Value {
    return .{ .cfunction = func };
}

pub fn value_from_module(index: ModuleIndex) Value {
    return .{ .module = index };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Operation = enum {
    add,
    sub,
    mul,
    div,
    mod,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const StringInternal = packed struct {
    offset: u32,
    len: u32,
};

pub const String = enum(u64) {
    null_string = 0,
    _,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const ModuleIndex = enum(u32) { _ };
pub const FunctionIndex = enum(u32) { _ };
pub const TableIndex = enum(u32) { _ };

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Stack = std.ArrayListUnmanaged(Value);

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Table = struct {
    moon: *Moon,
    array: std.ArrayListUnmanaged(Value) = .empty,
    hash: std.AutoHashMapUnmanaged(Value, Value) = .empty,

    pub fn deinit(self: *Table) void {
        self.array.deinit(self.moon.allocator);
        self.hash.deinit(self.moon.allocator);
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn tokenize(source: []const u8) TokenIterator {
    return .{
        .index = 0,
        .source = source,
    };
}

pub const TokenIterator = struct {
    index: usize,
    source: []const u8,
    current_token: ?Token = null,
    trace: bool = false,

    const State = enum {
        whitespace,
        identifier,
        integer,
        number,
        zero,
        hex_digits,
        plus,
        minus,
        star,
        slash,
        percent,
        tilde,
        open_block,
        close_block,
        open_round,
        close_round,
        equals,
        less_than,
        greater_than,
        string_literal,
        string_escaped,
        ampersand,
        bar,
        hat,
        bang,
        comma,
        cr,
        dot,
        semicolon,
        builtin,
        literal_identifier,
        literal_identifier_escaped,
        comment,
    };

    pub fn peek(self: *TokenIterator) ?Token {
        if (self.current_token) |tk| {
            return tk;
        }
        self.current_token = self.next_token();
        if (self.trace) {
            self.debug();
        }
        return self.current_token;
    }

    pub fn next(self: *TokenIterator) ?Token {
        self.current_token = self.next_token();
        if (self.trace) {
            self.debug();
        }
        return self.current_token;
    }

    pub fn skip_whitespace(self: *TokenIterator) void {
        while (self.peek()) |tk| {
            switch (tk.kind) {
                .eol => {
                    if (self.trace) {
                        self.debug();
                    }
                    _ = self.next();
                },
                else => {
                    break;
                },
            }
        }
    }

    pub fn debug(self: *TokenIterator) void {
        std.debug.print("TokenIterator({}/{} {?})\n", .{
            self.index,
            self.source.len,
            self.current_token,
        });
    }

    pub fn next_token(self: *TokenIterator) ?Token {
        var start = self.index;
        next_token: switch (State.whitespace) {
            .whitespace => {
                if (self.index >= self.source.len) return null;
                const ch = self.source[self.index];
                switch (ch) {
                    'a'...'z', 'A'...'Z', '_' => {
                        self.index += 1;
                        continue :next_token .identifier;
                    },
                    ' ', '\t', '\r' => {
                        self.index += 1;
                        start = self.index;
                        continue :next_token .whitespace;
                    },
                    '1'...'9' => {
                        self.index += 1;
                        continue :next_token .integer;
                    },
                    '0' => {
                        self.index += 1;
                        continue :next_token .zero;
                    },
                    '+' => {
                        self.index += 1;
                        continue :next_token .plus;
                    },
                    '-' => {
                        self.index += 1;
                        continue :next_token .minus;
                    },
                    '*' => {
                        self.index += 1;
                        continue :next_token .star;
                    },
                    '/' => {
                        self.index += 1;
                        continue :next_token .slash;
                    },
                    '%' => {
                        self.index += 1;
                        continue :next_token .percent;
                    },
                    '=' => {
                        self.index += 1;
                        continue :next_token .equals;
                    },
                    '<' => {
                        self.index += 1;
                        continue :next_token .less_than;
                    },
                    '>' => {
                        self.index += 1;
                        continue :next_token .greater_than;
                    },
                    '(' => {
                        self.index += 1;
                        continue :next_token .open_round;
                    },
                    ')' => {
                        self.index += 1;
                        continue :next_token .close_round;
                    },
                    '{' => {
                        self.index += 1;
                        continue :next_token .open_block;
                    },
                    '}' => {
                        self.index += 1;
                        continue :next_token .close_block;
                    },
                    '&' => {
                        self.index += 1;
                        continue :next_token .ampersand;
                    },
                    '|' => {
                        self.index += 1;
                        continue :next_token .bar;
                    },
                    '^' => {
                        self.index += 1;
                        continue :next_token .hat;
                    },
                    '~' => {
                        self.index += 1;
                        continue :next_token .tilde;
                    },
                    '!' => {
                        self.index += 1;
                        continue :next_token .bang;
                    },
                    '"' => {
                        self.index += 1;
                        continue :next_token .string_literal;
                    },
                    ';' => {
                        self.index += 1;
                        continue :next_token .semicolon;
                    },
                    ',' => {
                        self.index += 1;
                        continue :next_token .comma;
                    },
                    '.' => {
                        self.index += 1;
                        continue :next_token .dot;
                    },
                    '\n' => {
                        self.index += 1;
                        continue :next_token .cr;
                    },
                    '@' => {
                        self.index += 1;
                        continue :next_token .builtin;
                    },
                    else => {
                        self.index += 1;
                        return .{
                            .kind = .invalid_character,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                }
            },
            .identifier => {
                if (self.index >= self.source.len) {
                    const str = self.source[start..self.index];
                    return .{
                        .kind = check_keyword(str),
                        .index = start,
                        .len = self.index - start,
                    };
                }
                const ch = self.source[self.index];
                switch (ch) {
                    'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                        self.index += 1;
                        continue :next_token .identifier;
                    },
                    else => {
                        const str = self.source[start..self.index];
                        return .{
                            .kind = check_keyword(str),
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                }
            },
            .integer => {
                if (self.index >= self.source.len) return .{
                    .kind = .integer,
                    .index = start,
                    .len = self.index - start,
                };
                const ch = self.source[self.index];
                switch (ch) {
                    '0'...'9', '_' => {
                        self.index += 1;
                        continue :next_token .integer;
                    },
                    '.' => {
                        self.index += 1;
                        continue :next_token .number;
                    },
                    else => return .{
                        .kind = .integer,
                        .index = start,
                        .len = self.index - start,
                    },
                }
            },
            .number => {
                if (self.index >= self.source.len) return .{
                    .kind = .number,
                    .index = start,
                    .len = self.index - start,
                };
                const ch = self.source[self.index];
                switch (ch) {
                    '0'...'9', '_' => {
                        self.index += 1;
                        continue :next_token .number;
                    },
                    else => return .{
                        .kind = .number,
                        .index = start,
                        .len = self.index - start,
                    },
                }
            },
            .zero => {
                if (self.index >= self.source.len) return .{
                    .kind = .integer,
                    .index = start,
                    .len = self.index - start,
                };
                const ch = self.source[self.index];
                switch (ch) {
                    'x' => {
                        self.index += 1;
                        continue :next_token .hex_digits;
                    },
                    '.' => {
                        self.index += 1;
                        continue :next_token .number;
                    },
                    else => {
                        return .{
                            .kind = .integer,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                }
            },
            .hex_digits => {
                if (self.index >= self.source.len) return .{
                    .kind = .integer,
                    .index = start,
                    .len = self.index - start,
                };
                const ch = self.source[self.index];
                switch (ch) {
                    '0'...'9', 'a'...'f', 'A'...'F', '_' => {
                        self.index += 1;
                        continue :next_token .hex_digits;
                    },
                    else => {
                        return .{
                            .kind = .integer,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                }
            },
            .plus => {
                return .{
                    .kind = .op_add,
                    .index = start,
                    .len = self.index - start,
                };
            },
            .minus => {
                return .{
                    .kind = .op_sub,
                    .index = start,
                    .len = self.index - start,
                };
            },
            .star => {
                return .{
                    .kind = .op_mul,
                    .index = start,
                    .len = self.index - start,
                };
            },
            .slash => {
                if (self.index >= self.source.len) return .{
                    .kind = .op_div,
                    .index = start,
                    .len = self.index - start,
                };
                const ch = self.source[self.index];
                switch (ch) {
                    '/' => {
                        self.index += 1;
                        continue :next_token .comment;
                    },
                    else => {
                        return .{
                            .kind = .op_div,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                }
                return .{
                    .kind = .op_div,
                    .index = start,
                    .len = self.index - start,
                };
            },
            .comment => {
                if (self.index >= self.source.len) {
                    start = self.index;
                    continue :next_token .whitespace;
                }
                const ch = self.source[self.index];
                switch (ch) {
                    '\n' => {
                        self.index += 1;
                        continue :next_token .whitespace;
                    },
                    else => {
                        self.index += 1;
                        continue :next_token .comment;
                    },
                }
            },
            .percent => {
                return .{
                    .kind = .op_mod,
                    .index = start,
                    .len = self.index - start,
                };
            },
            .tilde => {
                return .{
                    .kind = .op_com,
                    .index = start,
                    .len = self.index - start,
                };
            },
            .less_than => {
                if (self.index >= self.source.len) return .{
                    .kind = .op_lt,
                    .index = start,
                    .len = self.index - start,
                };
                const ch = self.source[self.index];
                switch (ch) {
                    '=' => {
                        self.index += 1;
                        return .{
                            .kind = .op_lte,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                    '<' => {
                        self.index += 1;
                        return .{
                            .kind = .op_lsh,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                    else => {
                        return .{
                            .kind = .op_lt,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                }
            },
            .greater_than => {
                if (self.index >= self.source.len) return .{
                    .kind = .op_gt,
                    .index = start,
                    .len = self.index - start,
                };
                const ch = self.source[self.index];
                switch (ch) {
                    '=' => {
                        self.index += 1;
                        return .{
                            .kind = .op_gte,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                    '>' => {
                        self.index += 1;
                        return .{
                            .kind = .op_rsh,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                    else => {
                        return .{
                            .kind = .op_gt,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                }
            },
            .equals => {
                if (self.index >= self.source.len) return .{
                    .kind = .op_eq,
                    .index = start,
                    .len = self.index - start,
                };
                const ch = self.source[self.index];
                switch (ch) {
                    '=' => {
                        self.index += 1;
                        return .{
                            .kind = .op_eq,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                    else => {
                        return .{
                            .kind = .op_assign,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                }
            },
            .bang => {
                if (self.index >= self.source.len) return .{
                    .kind = .invalid_character,
                    .index = start,
                    .len = self.index - start,
                };
                const ch = self.source[self.index];
                switch (ch) {
                    '=' => {
                        self.index += 1;
                        return .{
                            .kind = .op_neq,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                    else => {
                        return .{
                            .kind = .invalid_character,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                }
            },
            .open_round => {
                return .{
                    .kind = .open_round,
                    .index = start,
                    .len = self.index - start,
                };
            },
            .close_round => {
                return .{
                    .kind = .close_round,
                    .index = start,
                    .len = self.index - start,
                };
            },
            .open_block => {
                return .{
                    .kind = .open_block,
                    .index = start,
                    .len = self.index - start,
                };
            },
            .close_block => {
                return .{
                    .kind = .close_block,
                    .index = start,
                    .len = self.index - start,
                };
            },
            .ampersand => {
                return .{
                    .kind = .op_band,
                    .index = start,
                    .len = self.index - start,
                };
            },
            .bar => {
                return .{
                    .kind = .op_bor,
                    .index = start,
                    .len = self.index - start,
                };
            },
            .hat => {
                return .{
                    .kind = .op_bxor,
                    .index = start,
                    .len = self.index - start,
                };
            },
            .string_literal => {
                if (self.index >= self.source.len) return .{
                    .kind = .string_literal,
                    .index = start,
                    .len = self.index - start,
                };
                const ch = self.source[self.index];
                if (ch == '\\') {
                    self.index += 1;
                    continue :next_token .string_escaped;
                } else if (ch == '"') {
                    self.index += 1;
                    return .{
                        .kind = .string_literal,
                        .index = start + 1,
                        .len = self.index - start - 2,
                    };
                } else {
                    self.index += 1;
                    continue :next_token .string_literal;
                }
            },
            .string_escaped => {
                if (self.index >= self.source.len) return .{
                    .kind = .string_literal,
                    .index = start + 1,
                    .len = self.index - start - 1,
                };
                const ch = self.source[self.index];
                switch (ch) {
                    'n', 'r', 't', '\"', '\\', 'x' => {
                        self.index += 1;
                        continue :next_token .string_literal;
                    },
                    else => {
                        continue :next_token .string_literal;
                    },
                }
            },
            .semicolon => {
                return .{
                    .kind = .eos,
                    .index = start,
                    .len = self.index - start,
                };
            },
            .comma => {
                return .{
                    .kind = .comma,
                    .index = start,
                    .len = self.index - start,
                };
            },
            .dot => {
                if (self.index >= self.source.len) return .{
                    .kind = .dot,
                    .index = start,
                    .len = self.index - start,
                };
                const ch = self.source[self.index];
                switch (ch) {
                    '{' => {
                        self.index += 1;
                        return .{
                            .kind = .dot_block,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                    '0'...'9' => {
                        self.index += 1;
                        continue :next_token .number;
                    },
                    else => {
                        return .{
                            .kind = .dot,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                }
            },
            .cr => {
                return .{
                    .kind = .eol,
                    .index = start,
                    .len = self.index - start,
                };
            },
            .builtin => {
                if (self.index >= self.source.len) return .{
                    .kind = .identifier,
                    .index = start,
                    .len = self.index - start,
                };
                const ch = self.source[self.index];
                switch (ch) {
                    'a'...'z',
                    'A'...'Z',
                    '_',
                    => {
                        continue :next_token .identifier;
                    },
                    '"' => {
                        self.index += 1;
                        start = self.index;
                        continue :next_token .literal_identifier;
                    },
                    else => {
                        return .{
                            .kind = .identifier,
                            .index = start,
                            .len = self.index - start,
                        };
                    },
                }
            },
            .literal_identifier => {
                if (self.index >= self.source.len) return .{
                    .kind = .identifier,
                    .index = start,
                    .len = self.index - start,
                };
                const ch = self.source[self.index];
                if (ch == '\\') {
                    self.index += 1;
                    continue :next_token .literal_identifier_escaped;
                } else if (ch == '"') {
                    self.index += 1;
                    return .{
                        .kind = .identifier,
                        .index = start,
                        .len = self.index - start - 1,
                    };
                } else {
                    self.index += 1;
                    continue :next_token .literal_identifier;
                }
            },
            .literal_identifier_escaped => {
                if (self.index >= self.source.len) return .{
                    .kind = .identifier,
                    .index = start,
                    .len = self.index - start,
                };
                const ch = self.source[self.index];
                switch (ch) {
                    'n', 'r', 't', '\"', '\\', 'x' => {
                        self.index += 1;
                        continue :next_token .literal_identifier;
                    },
                    else => {
                        continue :next_token .literal_identifier;
                    },
                }
            },
        }
        self.index += 1;
        return .{
            .kind = .invalid_character,
            .index = start,
            .len = self.index - start,
        };
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn check_keyword(str: []const u8) TokenKind {
    switch (str.len) {
        2 => {
            if (std.mem.eql(u8, "fn", str)) return .keyword_fn;
            if (std.mem.eql(u8, "if", str)) return .keyword_if;
            if (std.mem.eql(u8, "or", str)) return .keyword_or;
        },
        3 => {
            if (std.mem.eql(u8, "and", str)) return .keyword_and;
            if (std.mem.eql(u8, "nil", str)) return .keyword_nil;
            if (std.mem.eql(u8, "not", str)) return .keyword_not;
            if (std.mem.eql(u8, "pub", str)) return .keyword_pub;
            if (std.mem.eql(u8, "var", str)) return .keyword_var;
        },
        4 => {
            if (std.mem.eql(u8, "elif", str)) return .keyword_elif;
            if (std.mem.eql(u8, "else", str)) return .keyword_else;
            if (std.mem.eql(u8, "true", str)) return .keyword_true;
        },
        5 => {
            if (std.mem.eql(u8, "break", str)) return .keyword_break;
            if (std.mem.eql(u8, "const", str)) return .keyword_const;
            if (std.mem.eql(u8, "while", str)) return .keyword_while;
            if (std.mem.eql(u8, "false", str)) return .keyword_false;
        },
        6 => {
            if (std.mem.eql(u8, "return", str)) return .keyword_return;
        },
        7 => {
            if (std.mem.eql(u8, "@import", str)) return .keyword_at_import;
        },
        8 => {
            if (std.mem.eql(u8, "continue", str)) return .keyword_continue;
        },
        else => {},
    }
    return .identifier;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Token = packed struct {
    kind: TokenKind,
    index: usize,
    len: usize,

    pub fn get_string(self: Token, source: []const u8) []const u8 {
        return source[self.index .. self.index + self.len];
    }

    pub fn format(self: Token, _: anytype, _: anytype, writer: anytype) !void {
        try writer.print("{s}", .{
            @tagName(self.kind),
        });
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const TokenKind = enum(u8) {
    invalid_character,
    identifier,
    integer,
    number,
    open_round,
    close_round,
    open_block,
    close_block,
    string_literal,
    op_assign,
    op_add,
    op_sub,
    op_mul,
    op_div,
    op_mod,
    op_lsh,
    op_rsh,
    op_neg,
    op_band,
    op_bor,
    op_bxor,
    op_eq,
    op_neq,
    op_lte,
    op_gte,
    op_lt,
    op_gt,
    op_com,
    dot,
    dot_block,
    comma,
    keyword_and,
    keyword_break,
    keyword_const,
    keyword_continue,
    keyword_elif,
    keyword_else,
    keyword_false,
    keyword_fn,
    keyword_if,
    keyword_not,
    keyword_nil,
    keyword_or,
    keyword_pub,
    keyword_return,
    keyword_true,
    keyword_var,
    keyword_while,
    keyword_at_import,
    eol,
    eos,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn string_is_simple(str: []const u8) bool {
    for (str) |ch| {
        if (ch < 0x20 or ch == '\\' or ch >= 0x7f) return false;
    }
    return true;
}

fn expand_string(str: []const u8, writer: anytype) !void {
    var index: usize = 0;
    while (index < str.len) {
        const ch = str[index];
        if (ch == '\\') {
            index += 1;
            if (index < str.len) {
                switch (str[index]) {
                    'n' => try writer.writeByte('\n'),
                    'r' => try writer.writeByte('\r'),
                    't' => try writer.writeByte('\t'),
                    '"' => try writer.writeByte('\"'),
                    '\'' => try writer.writeByte('\''),
                    else => return error.InvalidCharacter,
                }
            }
        } else {
            try writer.writeByte(ch);
        }
        index += 1;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const lots_of_spaces = " " ** 256;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "init / deinit" {
    var moon = Moon.init(std.testing.allocator);
    defer moon.deinit();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "push pop" {
    var moon = Moon.init(std.testing.allocator);
    defer moon.deinit();

    try moon.push_integer(63);
    try moon.push_integer(42);
    try moon.push_integer(37);
    try moon.op(.add);
    const value = try moon.pop_integer();

    try std.testing.expectEqual(value, 79);

    const other = try moon.pop_integer();
    try std.testing.expectEqual(other, 63);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const TestTokenizeOptions = struct {
    trace: bool = false,
};

fn test_tokenize(
    str: []const u8,
    kinds: []const TokenKind,
    options: TestTokenizeOptions,
) !void {
    var iter = tokenize(str);
    var index: usize = 0;
    while (iter.next()) |tk| {
        try std.testing.expectEqual(kinds[index], tk.kind);
        if (options.trace) {
            std.debug.print("{s} \"{}\"\n", .{
                @tagName(tk.kind),
                std.zig.fmtEscapes(tk.get_string(str)),
            });
        }
        index += 1;
    }
    try std.testing.expectEqual(index, kinds.len);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const TestParseOptions = struct {
    trace_tokenize: bool = false,
    trace_parse: bool = false,
};

fn test_parse(source: []const u8, result: []const u8, options: TestParseOptions) !void {
    var moon = Moon.init(std.testing.allocator);
    defer moon.deinit();

    var iter = tokenize(source);
    iter.trace = options.trace_tokenize;

    var tree = moon.AST(source);
    defer tree.deinit();

    tree.trace = options.trace_parse;
    if (tree.trace) {
        std.debug.print("---------------\n", .{});
    }

    const root = try tree.parse(&iter);

    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    const writer = buffer.writer();

    try tree.dump(root, .{}, writer);

    try std.testing.expectEqualStrings(result, buffer.items);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn test_compile(str: []const u8, result: []const u8, options: CompileOptions) !void {
    var moon = Moon.init(std.testing.allocator);
    defer moon.deinit();

    try moon.add_std_module();

    moon.trace_execute = false;

    const module = try moon.compile(str, options);

    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    const writer = buffer.writer();

    // try moon.dump_module(module, writer);
    const index = @intFromEnum(module.module);
    const real_module = moon.modules.items[index];
    try moon.dump(writer);
    try moon.dump_module(real_module, writer);

    try std.testing.expectEqualStrings(result, buffer.items);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize symbols" {
    try test_tokenize("= + - * / % << >> & | ^ ~ == != <= >= < > , . .{", &[_]TokenKind{
        .op_assign,
        .op_add,
        .op_sub,
        .op_mul,
        .op_div,
        .op_mod,
        .op_lsh,
        .op_rsh,
        .op_band,
        .op_bor,
        .op_bxor,
        .op_com,
        .op_eq,
        .op_neq,
        .op_lte,
        .op_gte,
        .op_lt,
        .op_gt,
        .comma,
        .dot,
        .dot_block,
    }, .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize identifiers" {
    try test_tokenize("a b a_ _b _ a98 @\"while\" .", &[_]TokenKind{
        .identifier,
        .identifier,
        .identifier,
        .identifier,
        .identifier,
        .identifier,
        .identifier,
        .dot,
    }, .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize numbers and integers" {
    try test_tokenize("0 1 0x12 3. .3 0. 0.0 1.0 3.14 .", &[_]TokenKind{
        .integer,
        .integer,
        .integer,
        .number,
        .number,
        .number,
        .number,
        .number,
        .number,
        .dot,
    }, .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize keywords" {
    try test_tokenize(
        \\if elif else not and or while break continue return
        \\@import
        \\true false nil
    , &[_]TokenKind{
        .keyword_if,
        .keyword_elif,
        .keyword_else,
        .keyword_not,
        .keyword_and,
        .keyword_or,
        .keyword_while,
        .keyword_break,
        .keyword_continue,
        .keyword_return,
        .eol,
        .keyword_at_import,
        .eol,
        .keyword_true,
        .keyword_false,
        .keyword_nil,
    }, .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize non-keywords" {
    try test_tokenize("@\"if\"", &[_]TokenKind{
        .identifier,
    }, .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize call" {
    try test_tokenize("print (\"Hello, World!\n\")", &[_]TokenKind{
        .identifier,
        .open_round,
        .string_literal,
        .close_round,
    }, .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize variables" {
    try test_tokenize("const a = 37\nvar b = 42; var c = a + b", &[_]TokenKind{
        .keyword_const,
        .identifier,
        .op_assign,
        .integer,
        .eol,
        .keyword_var,
        .identifier,
        .op_assign,
        .integer,
        .eos,
        .keyword_var,
        .identifier,
        .op_assign,
        .identifier,
        .op_add,
        .identifier,
    }, .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize function def" {
    try test_tokenize("pub fn add (a, b) { const c = a + b; return c }", &[_]TokenKind{
        .keyword_pub,
        .keyword_fn,
        .identifier,
        .open_round,
        .identifier,
        .comma,
        .identifier,
        .close_round,
        .open_block,
        .keyword_const,
        .identifier,
        .op_assign,
        .identifier,
        .op_add,
        .identifier,
        .eos,
        .keyword_return,
        .identifier,
        .close_block,
    }, .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize comments" {
    try test_tokenize(
        \\1// one
        \\   // empty
        \\+//plus
        \\2// two
    , &[_]TokenKind{
        .integer,
        .op_add,
        .integer,
    }, .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "parse simple" {
    try test_parse("37 + 42 - 12",
        \\AST
        \\  block
        \\    op_sub
        \\      op_add
        \\        integer_literal 37
        \\        integer_literal 42
        \\      integer_literal 12
        \\
    , .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "parse mul add" {
    try test_parse("37 + 42 * 12",
        \\AST
        \\  block
        \\    op_add
        \\      integer_literal 37
        \\      op_mul
        \\        integer_literal 42
        \\        integer_literal 12
        \\
    , .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "parse mul add brackets" {
    try test_parse(
        \\ 3.7 + 4.2 * (.12 - 2.) / 3
        \\
    ,
        \\AST
        \\  block
        \\    op_add
        \\      number_literal 3.7
        \\      op_div
        \\        op_mul
        \\          number_literal 4.2
        \\          op_sub
        \\            number_literal 0.12
        \\            number_literal 2
        \\        integer_literal 3
        \\
    , .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "parse operator precedence" {
    try test_parse(
        \\ 1 < 2 & 3 >> 4 + 5 * 6
        \\ 1 * 2 + 3 << 4 | 5 > 6
        \\
    ,
        \\AST
        \\  block
        \\    op_lt
        \\      integer_literal 1
        \\      op_band
        \\        integer_literal 2
        \\        op_rsh
        \\          integer_literal 3
        \\          op_add
        \\            integer_literal 4
        \\            op_mul
        \\              integer_literal 5
        \\              integer_literal 6
        \\    op_gt
        \\      op_bor
        \\        op_lsh
        \\          op_add
        \\            op_mul
        \\              integer_literal 1
        \\              integer_literal 2
        \\            integer_literal 3
        \\          integer_literal 4
        \\        integer_literal 5
        \\      integer_literal 6
        \\
    , .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "parse logical operator precedence" {
    try test_parse(
        \\ 3 < 4 and 5 <= 6 or 7 > 8 or
        \\       9 >= 10 and 11 != 12 or 13 == 14
        \\
    ,
        \\AST
        \\  block
        \\    op_or
        \\      op_or
        \\        op_or
        \\          op_and
        \\            op_lt
        \\              integer_literal 3
        \\              integer_literal 4
        \\            op_lte
        \\              integer_literal 5
        \\              integer_literal 6
        \\          op_gt
        \\            integer_literal 7
        \\            integer_literal 8
        \\        op_and
        \\          op_gte
        \\            integer_literal 9
        \\            integer_literal 10
        \\          op_neq
        \\            integer_literal 11
        \\            integer_literal 12
        \\      op_eq
        \\        integer_literal 13
        \\        integer_literal 14
        \\
    , .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "parse unary operators" {
    try test_parse(
        \\ not ((-3) + (~4) < 5)
        \\
    ,
        \\AST
        \\  block
        \\    op_not
        \\      op_lt
        \\        op_add
        \\          op_neg
        \\            integer_literal 3
        \\          op_com
        \\            integer_literal 4
        \\        integer_literal 5
        \\
    , .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "parse statements" {
    try test_parse(
        \\ var a = 3
        \\ const b = 4
        \\ const c = a + b
        \\
    ,
        \\AST
        \\  block
        \\    var_decl a
        \\      integer_literal 3
        \\    const_decl b
        \\      integer_literal 4
        \\    const_decl c
        \\      op_add
        \\        identifier a
        \\        identifier b
        \\
    , .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "parse while" {
    try test_parse(
        \\ var a = 0
        \\ while (a < 4) { a = a + 1 }
        \\
    ,
        \\AST
        \\  block
        \\    var_decl a
        \\      integer_literal 0
        \\    while_stmt
        \\      op_lt
        \\        identifier a
        \\        integer_literal 4
        \\      block
        \\        assignment
        \\          identifier a
        \\          op_add
        \\            identifier a
        \\            integer_literal 1
        \\
    , .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "parse call" {
    try test_parse(
        \\ print (
        \\   "Hello, World!\n",
        \\   32,
        \\   -33,
        \\   ~34,
        \\   true,
        \\   false,
        \\   not true,
        \\ );
        \\
    ,
        \\AST
        \\  block
        \\    call
        \\      identifier print
        \\      arg
        \\        string "Hello, World!\\n"
        \\      arg
        \\        integer_literal 32
        \\      arg
        \\        op_neg
        \\          integer_literal 33
        \\      arg
        \\        op_com
        \\          integer_literal 34
        \\      arg
        \\        boolean_true
        \\      arg
        \\        boolean_false
        \\      arg
        \\        op_not
        \\          boolean_true
        \\
    , .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "parse dots" {
    try test_parse(
        \\ const dist = std.math.sqrt (pos.x * pos.x + pos.y * pos.y)
        \\
    ,
        \\AST
        \\  block
        \\    const_decl dist
        \\      call
        \\        op_dot
        \\          op_dot
        \\            identifier std
        \\            identifier math
        \\          identifier sqrt
        \\        arg
        \\          op_add
        \\            op_mul
        \\              op_dot
        \\                identifier pos
        \\                identifier x
        \\              op_dot
        \\                identifier pos
        \\                identifier x
        \\            op_mul
        \\              op_dot
        \\                identifier pos
        \\                identifier y
        \\              op_dot
        \\                identifier pos
        \\                identifier y
        \\
    , .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "parse if" {
    try test_parse(
        \\ if a < b
        \\ {
        \\     d.c = 42
        \\ }
        \\ elif a == b
        \\ {
        \\     d.c = 37
        \\ }
        \\ else
        \\ {
        \\     d.c = 0
        \\ }
        \\
    ,
        \\AST
        \\  block
        \\    if_stmt
        \\      op_lt
        \\        identifier a
        \\        identifier b
        \\      block
        \\        assignment
        \\          op_dot
        \\            identifier d
        \\            identifier c
        \\          integer_literal 42
        \\      op_eq
        \\        identifier a
        \\        identifier b
        \\      block
        \\        assignment
        \\          op_dot
        \\            identifier d
        \\            identifier c
        \\          integer_literal 37
        \\      block
        \\        assignment
        \\          op_dot
        \\            identifier d
        \\            identifier c
        \\          integer_literal 0
        \\
    , .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "parse function declaration" {
    try test_parse(
        \\ fn add (a, b) {
        \\     return a + b
        \\ }
        \\ fn eol () {
        \\     print ("\n")
        \\ }
        \\
    ,
        \\AST
        \\  block
        \\    func_decl add (a, b)
        \\      block
        \\        return_stmt
        \\          op_add
        \\            identifier a
        \\            identifier b
        \\    func_decl eol ()
        \\      block
        \\        call
        \\          identifier print
        \\          arg
        \\            string "\\n"
        \\
    , .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "parse table array" {
    try test_parse(
        \\ const empty = .{}
        \\ const names = .{ "Alice", "Bob", "Charlie" }
        \\ const look = .{ "Alice" = 3, "Bob" = 4, "Charlie" = 2 }
        \\ const mixed = .{ 3 + 4, "Bob", "x" = 5, 5 = "x", 7 + 9 = 16 }
        \\ const dot = .{ .x = 7, .y = 6 }
        \\
    ,
        \\AST
        \\  block
        \\    const_decl empty
        \\      table_decl
        \\    const_decl names
        \\      table_decl
        \\        value
        \\          string "Alice"
        \\        value
        \\          string "Bob"
        \\        value
        \\          string "Charlie"
        \\    const_decl look
        \\      table_decl
        \\        key
        \\          string "Alice"
        \\        value
        \\          integer_literal 3
        \\        key
        \\          string "Bob"
        \\        value
        \\          integer_literal 4
        \\        key
        \\          string "Charlie"
        \\        value
        \\          integer_literal 2
        \\    const_decl mixed
        \\      table_decl
        \\        value
        \\          op_add
        \\            integer_literal 3
        \\            integer_literal 4
        \\        value
        \\          string "Bob"
        \\        key
        \\          string "x"
        \\        value
        \\          integer_literal 5
        \\        key
        \\          integer_literal 5
        \\        value
        \\          string "x"
        \\        key
        \\          op_add
        \\            integer_literal 7
        \\            integer_literal 9
        \\        value
        \\          integer_literal 16
        \\    const_decl dot
        \\      table_decl
        \\        key
        \\          identifier x
        \\        value
        \\          integer_literal 7
        \\        key
        \\          identifier y
        \\        value
        \\          integer_literal 6
        \\
    , .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "parse hello world" {
    try test_parse(
        \\ const std = @import ("std")
        \\ std.print ("Hello, World!\n", .{})
        \\
    ,
        \\AST
        \\  block
        \\    const_decl std
        \\      call
        \\        at_import
        \\        arg
        \\          string "std"
        \\    call
        \\      op_dot
        \\        identifier std
        \\        identifier print
        \\      arg
        \\        string "Hello, World!\\n"
        \\      arg
        \\        table_decl
        \\
    , .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "parse block" {
    try test_parse(
        \\ const std = @import("std");
        \\ {
        \\    const builtin = @import ("builtin");
        \\ }
        \\ std.print ("Hello, World!\n");
        \\
    ,
        \\AST
        \\  block
        \\    const_decl std
        \\      call
        \\        at_import
        \\        arg
        \\          string "std"
        \\    block
        \\      const_decl builtin
        \\        call
        \\          at_import
        \\          arg
        \\            string "builtin"
        \\    call
        \\      op_dot
        \\        identifier std
        \\        identifier print
        \\      arg
        \\        string "Hello, World!\\n"
        \\
    , .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "opcode size and format" {
    try std.testing.expectEqual(
        @bitOffsetOf(ShortInstruction, "kind"),
        @bitOffsetOf(LongInstruction, "kind"),
    );
    try std.testing.expectEqual(2, @sizeOf(Instruction));
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "compile simple" {
    try test_compile(
        \\ fn add (a, b)
        \\ {
        \\     return a + b
        \\ }
        \\
        \\ const c = add (37, 42)
        \\
    ,
        \\moon
        \\  tables
        \\  modules
        \\    0: "std"
        \\    1: stdin
        \\module
        \\  globals
        \\    0: "add" function = function#0
        \\    1: "c" constant = 79
        \\  function#0 (2 params)
        \\    code
        \\      000000 load_local 0
        \\      000001 load_local 1
        \\      000002 add
        \\      000003 ret
        \\    constants
        \\
    ,
        .{
            .trace_code = false,
        },
    );
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "compile main hello world" {
    try test_compile(
        \\ const std = @import ("std")
        \\ fn main () {
        \\     std.print ("Hello, World!\n")
        \\ }
        \\
    ,
        \\moon
        \\  tables
        \\  modules
        \\    0: "std"
        \\    1: stdin
        \\module
        \\  globals
        \\    0: "std" constant = module#0
        \\    1: "main" function = function#0
        \\  function#0 (0 params)
        \\    code
        \\      000000 load_global globals.0 ; "std"
        \\      000001 load_table_k constants.0 ; "print"
        \\      000002 constant constants.1 ; "Hello, World!\n"
        \\      000003 call 1
        \\      000004 drop 1
        \\      000005 nil
        \\      000006 ret
        \\    constants
        \\      0: "print"
        \\      1: "Hello, World!\n"
        \\
    ,
        .{
            .trace_code = false,
        },
    );
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "compile hello world" {
    try test_compile(
        \\ const std = @import ("std")
        \\ std.print ("Hello, World!\n")
        \\
    ,
        \\moon
        \\  tables
        \\  modules
        \\    0: "std"
        \\    1: stdin
        \\module
        \\  globals
        \\    0: "std" constant = module#0
        \\
    ,
        .{
            .trace_code = false,
        },
    );
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "compile maths function" {
    try test_compile(
        \\ const std = @import ("std")
        \\ const a = std.sqrt (12.25)
        \\
    ,
        \\moon
        \\  tables
        \\  modules
        \\    0: "std"
        \\    1: stdin
        \\module
        \\  globals
        \\    0: "std" constant = module#0
        \\    1: "a" constant = 3.5
        \\
    ,
        .{
            .trace_code = false,
        },
    );
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn moon_std_print(moon: *Moon) callconv(.C) bool {
    if (moon.to_string(0)) |str| {
        const stdout = std.io.getStdOut ();
        const writer = stdout.writer ();
        writer.print("{s}", .{str}) catch {};
    }

    return false;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn moon_std_sqrt(moon: *Moon) callconv(.C) bool {
    if (moon.to_number(0)) |num| {
        moon.push_number(@sqrt(num)) catch return false;
        return true;
    }

    return false;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
