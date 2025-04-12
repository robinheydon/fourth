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

pub const Moon = struct {
    allocator: std.mem.Allocator,

    modules: std.ArrayListUnmanaged(*Module) = .empty,

    stack: std.ArrayListUnmanaged(Value) = .empty,

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
        self.stack.deinit(self.allocator);
    }

    pub fn push_integer(self: *Moon, value: i64) MoonErrors!void {
        try self.stack.append(self.allocator, .{ .integer = value });
    }

    pub fn op(self: *Moon, operation: Operation) MoonErrors!void {
        switch (operation) {
            .add => {
                if (self.stack.pop()) |rhs| {
                    if (self.stack.pop()) |lhs| {
                        const res = try self.add(lhs, rhs);
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

    pub fn compile(
        self: *Moon,
        source: []const u8,
        options: CompileOptions,
    ) MoonErrors!Value {
        const module = try self.generate_module(source, options);

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

    fn generate_module(
        self: *Moon,
        source: []const u8,
        options: CompileOptions,
    ) MoonErrors!*Module {
        var iter = tokenize(source);

        var tree = self.AST();
        defer tree.deinit();

        const root = try tree.parse(&iter);

        if (options.trace_code) {
            var buffer = std.ArrayList(u8).init(std.testing.allocator);
            defer buffer.deinit();

            const writer = buffer.writer();
            try tree.dump(root, .{
                .show_node_index = true,
            }, writer);
            std.debug.print("{s}\n", .{buffer.items});
        }

        var module = try self.allocator.create(Module);
        module.* = .{
            .moon = self,
            .code = .empty,
            .constants = .empty,
            .globals = .empty,
            .strings = .empty,
            .locals = .empty,
            .trace = options.trace_code,
        };
        errdefer {
            module.deinit();
            self.allocator.destroy(module);
        }

        // _ = try module.add_constant_string(source);

        try module.semantic_analysis(&tree, root);

        module.global_scope = true;
        try module.generate_code_block(&tree, root);
        module.global_scope = false;

        _ = try module.add_code(.ret, 0);

        var giter = module.globals.iterator();
        while (giter.next()) |*global| {
            const value = global.value_ptr;
            switch (value.*) {
                .function => |*func| {
                    if (func.need_generation) {
                        func.start = module.get_code_addr();
                        try module.generate_code_block(&tree, func.node_index);
                        func.end = module.get_code_addr();
                        func.need_generation = false;
                    }
                },
                else => {},
            }
        }

        return module;
    }

    pub fn AST(self: *Moon) Moon_AST {
        return .{
            .moon = self,
        };
    }

    pub fn add(self: *Moon, lhs: Value, rhs: Value) MoonErrors!Value {
        _ = self;
        switch (lhs) {
            .integer => |l| {
                switch (rhs) {
                    .integer => |r| {
                        return .{ .integer = l + r };
                    },
                    else => {},
                }
            },
            else => {},
        }
        return error.TypeMismatch;
    }

    pub fn dump(self: *Moon, value: Value, writer: anytype) !void {
        switch (value) {
            .nil => try writer.writeAll("nil"),
            .integer => |i| try writer.print("{}", .{i}),
            .number => |n| try writer.print("{d}", .{n}),
            .string => |s| {
                // const str = self.get_string (s);
                // try writer.print ("\"{}\"", .{std.zig.fmtEscapes (str)});
                try writer.print("string#{}", .{s});
            },
            .function => |f| {
                const index = @intFromEnum(f);
                try writer.print("func#{}", .{index});
            },
            .module => |m| {
                const index = @intFromEnum(m);
                const module = self.modules.items[index];
                try self.dump_module(module, writer);
            },
        }
    }

    fn dump_module(self: *Moon, module: *Module, writer: anytype) !void {
        try writer.print("module\n", .{});

        try writer.print("  code\n", .{});
        try self.dump_code(module, module.code.items, writer);

        try writer.print("  constants\n", .{});
        for (module.constants.items, 0..) |constant, index| {
            try writer.print("    {}: ", .{index});
            switch (constant) {
                .integer => |value| try writer.print("integer: {x}", .{value}),
                .number => |value| try writer.print("number: {d}", .{value}),
                .string => |value| try writer.print("string: \"{}\"", .{
                    std.zig.fmtEscapes(
                        module.strings.items[value.offset .. value.offset + value.len],
                    ),
                }),
            }
            try writer.print("\n", .{});
        }
        try writer.print("  globals\n", .{});
        var giter = module.globals.iterator();
        while (giter.next()) |kv| {
            const key = kv.key_ptr.*;
            const value = kv.value_ptr.*;
            switch (value) {
                .constant => |c| {
                    try writer.print("    {}: \"{}\" constant\n", .{
                        c.index,
                        std.zig.fmtEscapes(key),
                    });
                },
                .variable => |v| {
                    try writer.print("    {}: \"{}\" variable\n", .{
                        v.index,
                        std.zig.fmtEscapes(key),
                    });
                },
                .function => |f| {
                    try writer.print("    {}: \"{}\" function {x:0>6}\n", .{
                        f.index,
                        std.zig.fmtEscapes(key),
                        @intFromEnum(f.start),
                    });
                },
            }
        }
        try writer.print("  strings\n", .{});

        for (0..module.strings.items.len) |index| {
            const start = index * 32;
            if (start >= module.strings.items.len) break;

            const end = @min(module.strings.items.len, index * 32 + 32);
            try writer.print("    {x:0>8} \"{}\"\n", .{
                start,
                std.zig.fmtEscapes(module.strings.items[start..end]),
            });
        }
    }

    fn dump_code(
        self: *Moon,
        module: *Module,
        instructions: []const Instruction,
        writer: anytype,
    ) !void {
        _ = self;
        for (instructions, 0..) |instruction, addr| {
            switch (instruction.len()) {
                .short => {
                    const code = instruction.get_short();
                    switch (code.op) {
                        .constant,
                        .jmpk,
                        .brak,
                        => {
                            const name = @tagName(code.op);
                            try writer.print("    {x:0>6} {s} {}\n", .{
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
                            try writer.print("    {x:0>6} {s} {}\n", .{ addr, name, iarg });
                        },
                    }
                },
                .long => {
                    const code = instruction.get_long();
                    switch (code.op) {
                        .nop,
                        .boolean,
                        .integer,
                        .load_local,
                        .store_local,
                        .call,
                        .import,
                        .drop,
                        => {
                            try writer.print("    {x:0>6} {s} {}\n", .{
                                addr,
                                @tagName(code.op),
                                code.arg,
                            });
                        },
                        .load_table,
                        .store_table,
                        => {
                            const constant = module.constants.items[code.arg];
                            switch (constant) {
                                .string => |s| {
                                    const start = s.offset;
                                    const end = s.offset + s.len;
                                    const name = module.strings.items[start..end];
                                    try writer.print("    {x:0>6} {s} {} ; \"{}\"\n", .{
                                        addr,
                                        @tagName(code.op),
                                        code.arg,
                                        std.zig.fmtEscapes(name),
                                    });
                                },
                                else => {
                                    try writer.print("    {x:0>6} {s} {}\n", .{
                                        addr,
                                        @tagName(code.op),
                                        code.arg,
                                    });
                                },
                            }
                        },
                        .load_global,
                        .store_global,
                        => {
                            if (module.get_global_index_name(code.arg)) |name| {
                                try writer.print("    {x:0>6} {s} {} ; \"{}\"\n", .{
                                    addr,
                                    @tagName(code.op),
                                    code.arg,
                                    std.zig.fmtEscapes(name),
                                });
                            } else {
                                try writer.print("    {x:0>6} {s} {}\n", .{
                                    addr,
                                    @tagName(code.op),
                                    code.arg,
                                });
                            }
                        },
                        .nil,
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
                            try writer.print("    {x:0>6} {s}\n", .{
                                addr,
                                @tagName(code.op),
                            });
                        },
                    }
                },
            }
        }
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const CodeAddr = enum(u24) { _ };

const GlobalKind = enum {
    variable,
    constant,
    function,
};

const Global = union(GlobalKind) {
    variable: GlobalVariable,
    constant: GlobalConstant,
    function: GlobalFunction,
};

const GlobalConstant = struct {
    index: u32,
};

const GlobalVariable = struct {
    index: u32,
};

const GlobalFunction = struct {
    index: u32,
    need_generation: bool = true,
    node_index: NodeIndex,
    start: CodeAddr = @enumFromInt(0),
    end: CodeAddr = @enumFromInt(0),
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const LocalKind = enum {
    constant,
    variable,
};

const Local = struct {
    name: []const u8,
    kind: LocalKind,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const Module = struct {
    moon: *Moon,
    globals: std.StringArrayHashMapUnmanaged(Global),
    code: std.ArrayListUnmanaged(Instruction),
    constants: std.ArrayListUnmanaged(Constant),
    strings: std.ArrayListUnmanaged(u8),
    next_global: u32 = 0,
    global_scope: bool = false,
    locals: std.ArrayListUnmanaged(Local),
    trace: bool = false,

    pub fn deinit(self: *Module) void {
        self.globals.deinit(self.moon.allocator);
        self.code.deinit(self.moon.allocator);
        self.constants.deinit(self.moon.allocator);
        self.strings.deinit(self.moon.allocator);
        self.locals.deinit(self.moon.allocator);
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
                try self.declare_global(decl.name, .constant, node_index);
            },
            .var_decl => |decl| {
                try self.declare_global(decl.name, .variable, node_index);
            },
            .func_decl => |decl| {
                try self.declare_global(decl.name, .function, decl.block);
                for (decl.params) |param| {
                    try self.declare_local(param.name, .constant);
                }
            },
            else => {
                // std.debug.print("Statement: {s}\n", .{@tagName(node)});
            },
        }
    }

    pub fn declare_global(
        self: *Module,
        name: []const u8,
        kind: GlobalKind,
        node_index: NodeIndex,
    ) MoonErrors!void {
        if (self.find_global(name)) |_| {
            std.debug.print("{s} already declared (declare global / global)\n", .{name});
            return error.NameAlreadyDeclared;
        }
        if (self.find_local(name)) |_| {
            std.debug.print("{s} already declared (declare global / local)\n", .{name});
            return error.NameAlreadyDeclared;
        }
        switch (kind) {
            .constant => {
                try self.globals.put(self.moon.allocator, name, .{ .constant = .{
                    .index = self.next_global,
                } });
            },
            .variable => {
                try self.globals.put(self.moon.allocator, name, .{ .variable = .{
                    .index = self.next_global,
                } });
            },
            .function => {
                try self.globals.put(self.moon.allocator, name, .{ .function = .{
                    .index = self.next_global,
                    .node_index = node_index,
                } });
            },
        }
        self.next_global += 1;
    }

    pub fn find_global(self: *Module, name: []const u8) ?u32 {
        if (self.globals.get(name)) |value| {
            switch (value) {
                .constant => |c| return c.index,
                .variable => |c| return c.index,
                .function => |c| return c.index,
            }
        }
        return null;
    }

    pub fn declare_local(self: *Module, name: []const u8, kind: LocalKind) !void {
        if (self.find_global(name)) |_| {
            std.debug.print("{s} already declared (declare_local / global)\n", .{name});
            return error.NameAlreadyDeclared;
        }
        if (self.find_local(name)) |_| {
            std.debug.print("{s} already declared (declare_local / local)\n", .{name});
            return error.NameAlreadyDeclared;
        }

        try self.locals.append(self.moon.allocator, .{
            .name = name,
            .kind = kind,
        });
    }

    pub fn find_local(self: *Module, name: []const u8) ?u32 {
        for (self.locals.items, 0..) |local, index| {
            if (local.name.len == name.len) {
                if (std.mem.eql(u8, name, local.name)) {
                    return @truncate(index);
                }
            }
        }
        return null;
    }

    pub fn find_local_variable(self: *Module, name: []const u8) ?u32 {
        for (self.locals.items, 0..) |local, index| {
            if (local.name.len == name.len) {
                if (std.mem.eql(u8, name, local.name)) {
                    if (local.kind == .variable) {
                        return @truncate(index);
                    }
                }
            }
        }
        return null;
    }

    pub fn find_local_constant(self: *Module, name: []const u8) ?u32 {
        for (self.locals.items, 0..) |local, index| {
            if (local.name.len == name.len) {
                if (std.mem.eql(u8, name, local.name)) {
                    if (local.kind == .constant) {
                        return @truncate(index);
                    }
                }
            }
        }
        return null;
    }

    pub fn get_global_index_name(self: *Module, index: usize) ?[]const u8 {
        var iter = self.globals.iterator();
        while (iter.next()) |kv| {
            const value = kv.value_ptr.*;
            switch (value) {
                .constant => |c| if (c.index == index) return kv.key_ptr.*,
                .variable => |c| if (c.index == index) return kv.key_ptr.*,
                .function => |c| if (c.index == index) return kv.key_ptr.*,
            }
        }
        return null;
    }

    pub fn generate_code_block(
        self: *Module,
        tree: *const Moon_AST,
        node_index: NodeIndex,
    ) MoonErrors!void {
        const index = node_index.as_usize();
        const node = tree.nodes.items[index];
        if (self.trace) {
            std.debug.print("generate_code_block {} {s}{s}\n", .{
                node_index,
                @tagName(node),
                if (self.global_scope) " global_scope" else "",
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
                if (!self.global_scope) {
                    try self.declare_local(decl.name, .constant);
                }
                try self.generate_code(tree, decl.expr, .{});
                if (self.find_global(decl.name)) |i| {
                    const arg: LongArg = @truncate(i);
                    _ = try self.add_code(.store_global, arg);
                } else if (self.find_local(decl.name)) |_| {
                    // const arg: LongArg = @truncate(i);
                    // _ = try self.add_code(.store_local, arg);
                } else {
                    return error.UnknownVariable;
                }
            },
            .var_decl => |decl| {
                if (!self.global_scope) {
                    try self.declare_local(decl.name, .variable);
                }
                try self.generate_code(tree, decl.expr, .{});
                if (self.find_global(decl.name)) |i| {
                    const arg: LongArg = @truncate(i);
                    _ = try self.add_code(.store_global, arg);
                } else if (self.find_local(decl.name)) |_| {
                    // const arg: LongArg = @truncate(i);
                    // _ = try self.add_code(.store_local, arg);
                } else {
                    return error.UnknownVariable;
                }
            },
            else => {
                try self.generate_code(tree, node_index, .{});
            },
        }
    }

    const GenerateCodeOptions = struct {
        assignment: bool = false,
    };

    pub fn generate_code(
        self: *Module,
        tree: *const Moon_AST,
        node_index: NodeIndex,
        options: GenerateCodeOptions,
    ) MoonErrors!void {
        const index = node_index.as_usize();
        const node = tree.nodes.items[index];
        if (self.trace) {
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
                    _ = try self.add_code(.store_table, arg);
                } else {
                    _ = try self.add_code(.load_table, arg);
                }
            },
            else => {},
        }
        // then generate the instruction
        switch (node) {
            .nil => _ = try self.add_code(.nil, 0),
            .boolean_true => _ = try self.add_code(.boolean, 1),
            .boolean_false => _ = try self.add_code(.boolean, 0),
            .integer_literal => |i| {
                if (i >= std.math.minInt(LongIArg) and i <= std.math.maxInt(LongIArg)) {
                    const value: LongArg = @bitCast(@as(LongIArg, @truncate(i)));
                    _ = try self.add_code(.integer, value);
                }
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
                    var buffer = std.ArrayList(u8).init(self.moon.allocator);
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
                if (self.is_builtin(tree, stmt.func)) {
                    try self.generate_call_builtin(tree, stmt.func, stmt.args);
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
                if (self.find_global(str)) |i| {
                    const arg: LongArg = @truncate(i);
                    if (options.assignment) {
                        _ = try self.add_code(.store_global, arg);
                    } else {
                        _ = try self.add_code(.load_global, arg);
                    }
                } else if (self.find_local_variable(str)) |i| {
                    const arg: LongArg = @truncate(i);
                    if (options.assignment) {
                        _ = try self.add_code(.store_local, arg);
                    } else {
                        _ = try self.add_code(.load_local, arg);
                    }
                } else if (self.find_local_constant(str)) |i| {
                    const arg: LongArg = @truncate(i);
                    if (options.assignment) {
                        std.debug.print("{s} constant\n", .{str});
                        return error.NotVariable;
                    } else {
                        _ = try self.add_code(.load_local, arg);
                    }
                } else {
                    std.debug.print("{s} unknown\n", .{str});
                    return error.UnknownVariable;
                }
            },
            .const_decl,
            .var_decl,
            => |decl| {
                try self.generate_code(tree, decl.expr, .{});
                if (self.find_global(decl.name)) |i| {
                    const arg: LongArg = @truncate(i);
                    _ = try self.add_code(.store_global, arg);
                } else if (self.find_local(decl.name)) |i| {
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
            .at_import => {
                _ = try self.add_code(.import, 0);
            },
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

    pub fn is_builtin(_: *Module, tree: *const Moon_AST, node_index: NodeIndex) bool {
        const index = node_index.as_usize();
        const node = tree.nodes.items[index];
        switch (node) {
            .at_import => return true,
            else => return false,
        }
    }

    pub fn generate_call_builtin(
        self: *Module,
        tree: *const Moon_AST,
        builtin: NodeIndex,
        args: []NodeIndex,
    ) MoonErrors!void {
        const index = builtin.as_usize();
        const node = tree.nodes.items[index];
        switch (node) {
            .at_import => {
                if (args.len != 1) {
                    return error.InvalidNumberOfArgs;
                }
                const arg_index = args[0].as_usize();
                const arg = tree.nodes.items[arg_index];
                if (arg != .string) {
                    return error.InvalidParameter;
                }
                const i = try self.add_constant_string(arg.string);
                if (i <= std.math.maxInt(ShortArg)) {
                    _ = try self.add_code(.import, @truncate(i));
                } else {
                    return error.TooManyConstants;
                }
            },
            else => {
                return error.InvalidBuiltin;
            },
        }
    }

    pub fn get_code_addr(self: *Module) CodeAddr {
        return @enumFromInt(self.code.items.len);
    }

    pub fn add_short_code(self: *Module, op: ShortOpcode, arg: ShortArg) MoonErrors!CodeAddr {
        const index: u24 = @intCast(self.code.items.len);
        try self.code.append(self.moon.allocator, Instruction.short(op, arg));
        return @enumFromInt(index);
    }

    pub fn add_code(self: *Module, op: LongOpcode, arg: LongArg) MoonErrors!CodeAddr {
        const index: u24 = @intCast(self.code.items.len);
        try self.code.append(self.moon.allocator, Instruction.long(op, arg));
        return @enumFromInt(index);
    }

    pub fn add_jmp(self: *Module, offset_addr: CodeAddr) MoonErrors!void {
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

    pub fn add_bra(self: *Module, offset_addr: CodeAddr) MoonErrors!void {
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
        self: *Module,
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

    pub fn add_constant_i64(self: *Module, value: i64) MoonErrors!u8 {
        const index: u8 = @intCast(self.constants.items.len);
        try self.constants.append(self.moon.allocator, .{
            .integer = value,
        });
        return index;
    }

    pub fn add_constant_f64(self: *Module, value: f64) MoonErrors!u8 {
        const index: u8 = @intCast(self.constants.items.len);
        try self.constants.append(self.moon.allocator, .{
            .number = value,
        });
        return index;
    }

    pub fn add_constant_string(self: *Module, value: []const u8) MoonErrors!u32 {
        const index: u32 = @intCast(self.constants.items.len);
        const len: u32 = @intCast(value.len);
        if (std.mem.indexOf(u8, self.strings.items, value)) |uoffset| {
            const offset: u32 = @intCast(uoffset);
            for (self.constants.items, 0..) |item, i| {
                switch (item) {
                    .string => |str| {
                        if (str.offset == offset and str.len == len) {
                            return @intCast(i);
                        }
                    },
                    else => {},
                }
            }
            try self.constants.append(self.moon.allocator, .{
                .string = .{
                    .offset = offset,
                    .len = len,
                },
            });
        } else {
            const offset: u32 = @intCast(self.strings.items.len);
            try self.strings.appendSlice(self.moon.allocator, value);
            try self.constants.append(self.moon.allocator, .{
                .string = .{
                    .offset = offset,
                    .len = len,
                },
            });
        }
        return index;
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
    load_table,
    store_table,
    boolean,
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

pub const ConstantKind = enum {
    integer,
    number,
    string,
};

pub const Constant = union(ConstantKind) {
    integer: i64,
    number: f64,
    string: ConstantString,
};

//     pub fn format(self: Constant, _: anytype, _: anytype, writer: anytype) !void {
//         switch (self) {
//             .integer => |value| try writer.print("integer: {x}", .{value}),
//             .number => |value| try writer.print("number: {d}", .{value}),
//             .string => |value| try writer.print("string: \"{}\" ({d}:{d})", .{
//                 std.zig.fmtEscapes(
//                 value.offset,
//                 value.len,
//             }),
//         }
//     }

pub const ConstantString = packed struct {
    offset: u32,
    len: u32,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const NodeIndex = enum(u32) {
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
                        const expr = try self.parse_expr(iter);
                        const stmt = try self.new_node(
                            .{ .return_stmt = expr },
                        );
                        try stmts.append(self.moon.allocator, stmt);
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

            const name = tk.str;

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

            const name = tk.str;

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
                const name = tk.str;
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
                        .name = ntk.str,
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

        return try self.parse_logical_or(iter);
    }

    pub fn parse_logical_or(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        if (self.trace) {
            const tk = iter.peek();
            std.debug.print("parse_logical_or: {?}\n", .{tk});
        }

        var lhs = try self.parse_logical_and(iter);
        while (iter.peek()) |op| {
            if (self.trace) {
                std.debug.print("parse_logical_or: {}\n", .{op});
            }
            if (op.kind == .keyword_or) {
                _ = iter.next();
                const rhs = try self.parse_logical_and(iter);
                lhs = try self.new_node(
                    .{ .op_or = .{ .lhs = lhs, .rhs = rhs } },
                );
            } else {
                break;
            }
        }
        return lhs;
    }

    pub fn parse_logical_and(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        if (self.trace) {
            const tk = iter.peek();
            std.debug.print("parse_logical_and: {?}\n", .{tk});
        }

        var lhs = try self.parse_comparative(iter);
        while (iter.peek()) |op| {
            if (self.trace) {
                std.debug.print("parse_logical_and: {}\n", .{op});
            }
            if (op.kind == .keyword_and) {
                _ = iter.next();
                const rhs = try self.parse_comparative(iter);
                lhs = try self.new_node(
                    .{ .op_and = .{ .lhs = lhs, .rhs = rhs } },
                );
            } else {
                break;
            }
        }
        return lhs;
    }

    pub fn parse_comparative(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        if (self.trace) {
            const tk = iter.peek();
            std.debug.print("parse_comparative: {?}\n", .{tk});
        }
        var lhs = try self.parse_addition(iter);
        if (iter.peek()) |op| {
            if (self.trace) {
                std.debug.print("parse_comparative: {}\n", .{op});
            }
            switch (op.kind) {
                .op_lt => {
                    _ = iter.next();
                    const rhs = try self.parse_addition(iter);
                    lhs = try self.new_node(
                        .{ .op_lt = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_lte => {
                    _ = iter.next();
                    const rhs = try self.parse_addition(iter);
                    lhs = try self.new_node(
                        .{ .op_lte = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_gt => {
                    _ = iter.next();
                    const rhs = try self.parse_addition(iter);
                    lhs = try self.new_node(
                        .{ .op_gt = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_gte => {
                    _ = iter.next();
                    const rhs = try self.parse_addition(iter);
                    lhs = try self.new_node(
                        .{ .op_gte = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_neq => {
                    _ = iter.next();
                    const rhs = try self.parse_addition(iter);
                    lhs = try self.new_node(
                        .{ .op_neq = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_eq => {
                    _ = iter.next();
                    const rhs = try self.parse_addition(iter);
                    lhs = try self.new_node(
                        .{ .op_eq = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                else => {},
            }
        }
        return lhs;
    }

    pub fn parse_addition(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        if (self.trace) {
            const tk = iter.peek();
            std.debug.print("parse_addition: {?}\n", .{tk});
        }

        var lhs = try self.parse_multiplication(iter);
        while (iter.peek()) |op| {
            if (self.trace) {
                std.debug.print("parse_addition: {}\n", .{op});
            }
            switch (op.kind) {
                .op_add => {
                    _ = iter.next();
                    const rhs = try self.parse_multiplication(iter);
                    lhs = try self.new_node(
                        .{ .op_add = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_sub => {
                    _ = iter.next();
                    const rhs = try self.parse_multiplication(iter);
                    lhs = try self.new_node(
                        .{ .op_sub = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_bor => {
                    _ = iter.next();
                    const rhs = try self.parse_multiplication(iter);
                    lhs = try self.new_node(
                        .{ .op_bor = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_bxor => {
                    _ = iter.next();
                    const rhs = try self.parse_multiplication(iter);
                    lhs = try self.new_node(
                        .{ .op_bxor = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                else => break,
            }
        }
        return lhs;
    }

    pub fn parse_multiplication(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!NodeIndex {
        if (self.trace) {
            const tk = iter.peek();
            std.debug.print("parse_multiplication: {?}\n", .{tk});
        }

        var lhs = try self.parse_unary(iter);
        while (iter.peek()) |op| {
            if (self.trace) {
                std.debug.print("parse_multiplication: {}\n", .{op});
            }
            switch (op.kind) {
                .op_mul => {
                    _ = iter.next();
                    const rhs = try self.parse_unary(iter);
                    lhs = try self.new_node(
                        .{ .op_mul = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_div => {
                    _ = iter.next();
                    const rhs = try self.parse_unary(iter);
                    lhs = try self.new_node(
                        .{ .op_div = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_mod => {
                    _ = iter.next();
                    const rhs = try self.parse_unary(iter);
                    lhs = try self.new_node(
                        .{ .op_mod = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_lsh => {
                    _ = iter.next();
                    const rhs = try self.parse_unary(iter);
                    lhs = try self.new_node(
                        .{ .op_lsh = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_rsh => {
                    _ = iter.next();
                    const rhs = try self.parse_unary(iter);
                    lhs = try self.new_node(
                        .{ .op_rsh = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                .op_band => {
                    _ = iter.next();
                    const rhs = try self.parse_unary(iter);
                    lhs = try self.new_node(
                        .{ .op_band = .{ .lhs = lhs, .rhs = rhs } },
                    );
                },
                else => break,
            }
        }
        return lhs;
    }

    pub fn parse_unary(self: *Moon_AST, iter: *TokenIterator) MoonErrors!NodeIndex {
        if (iter.peek()) |tk| {
            if (self.trace) {
                std.debug.print("parse_atom: {}\n", .{tk});
            }

            switch (tk.kind) {
                .keyword_not => {
                    _ = iter.next();
                    const rhs = try self.parse_unary(iter);
                    return try self.new_node(
                        .{ .op_not = rhs },
                    );
                },
                .op_sub => {
                    _ = iter.next();
                    const rhs = try self.parse_unary(iter);
                    return try self.new_node(
                        .{ .op_neg = rhs },
                    );
                },
                .op_com => {
                    _ = iter.next();
                    const rhs = try self.parse_unary(iter);
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
                    return try self.new_node(
                        .{ .integer_literal = try std.fmt.parseInt(i64, tk.str, 0) },
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
                        .{ .string = tk.str },
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
                .{ .identifier = tk.str },
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
                                    .{ .identifier = itk.str },
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
                            .identifier = ctk.str,
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

    pub fn is_expression(kind: Kind) bool {
        switch (kind) {
            .integer,
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

    const lots_of_spaces = " " ** 256;

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
                    try writer.print(" {s}\n", .{func.name});
                    for (func.params) |param| {
                        try writer.print("{s}", .{lots_of_spaces[0 .. depth * 2]});
                        try writer.print("  {s}\n", .{param.name});
                    }
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
    NotImplemented,
    NotInteger,
    OutOfMemory,
    StackUnderflow,
    Overflow,
    InvalidCharacter,
    TypeMismatch,
    InvalidExpression,
    InvalidExpressionAtom,
    InvalidStatement,
    InvalidVariableDeclaration,
    InvalidConstantDeclaration,
    InvalidFunctionDeclaration,
    InvalidParameterList,
    InvalidBuiltin,
    InvalidNumberOfArgs,
    InvalidParameter,
    MissingStatements,
    MissingIdentifier,
    MissingAssignment,
    MissingOpenBlock,
    MissingCloseBlock,
    MissingOpenParenthesis,
    MissingCloseParenthesis,
    TooManyConstants,
    UnknownVariable,
    NotVariable,
    NameAlreadyDeclared,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const ValueKind = enum {
    nil,
    integer,
    number,
    string,
    function,
    module,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Value = union(ValueKind) {
    nil,
    integer: i64,
    number: f64,
    string: StringIndex,
    function: FunctionIndex,
    module: ModuleIndex,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Operation = enum {
    add,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const StringIndex = enum(u32) { _ };

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const FunctionIndex = enum(u32) { _ };

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const ModuleIndex = enum(u32) { _ };

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Stack = std.ArrayListUnmanaged(Value);

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
                            .str = self.source[start..self.index],
                        };
                    },
                }
            },
            .identifier => {
                if (self.index >= self.source.len) {
                    const str = self.source[start..self.index];
                    return .{
                        .kind = check_keyword(str),
                        .str = str,
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
                            .str = str,
                        };
                    },
                }
            },
            .integer => {
                if (self.index >= self.source.len) return .{
                    .kind = .integer,
                    .str = self.source[start..self.index],
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
                        .str = self.source[start..self.index],
                    },
                }
            },
            .number => {
                if (self.index >= self.source.len) return .{
                    .kind = .number,
                    .str = self.source[start..self.index],
                };
                const ch = self.source[self.index];
                switch (ch) {
                    '0'...'9', '_' => {
                        self.index += 1;
                        continue :next_token .number;
                    },
                    else => return .{
                        .kind = .number,
                        .str = self.source[start..self.index],
                    },
                }
            },
            .zero => {
                if (self.index >= self.source.len) return .{
                    .kind = .integer,
                    .str = self.source[start..self.index],
                };
                const ch = self.source[self.index];
                switch (ch) {
                    'x' => {
                        self.index += 1;
                        continue :next_token .hex_digits;
                    },
                    else => {
                        return .{
                            .kind = .integer,
                            .str = self.source[start..self.index],
                        };
                    },
                }
            },
            .hex_digits => {
                if (self.index >= self.source.len) return .{
                    .kind = .integer,
                    .str = self.source[start..self.index],
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
                            .str = self.source[start..self.index],
                        };
                    },
                }
            },
            .plus => {
                return .{
                    .kind = .op_add,
                    .str = self.source[start..self.index],
                };
            },
            .minus => {
                return .{
                    .kind = .op_sub,
                    .str = self.source[start..self.index],
                };
            },
            .star => {
                return .{
                    .kind = .op_mul,
                    .str = self.source[start..self.index],
                };
            },
            .slash => {
                return .{
                    .kind = .op_div,
                    .str = self.source[start..self.index],
                };
            },
            .percent => {
                return .{
                    .kind = .op_mod,
                    .str = self.source[start..self.index],
                };
            },
            .tilde => {
                return .{
                    .kind = .op_com,
                    .str = self.source[start..self.index],
                };
            },
            .less_than => {
                if (self.index >= self.source.len) return .{
                    .kind = .op_lt,
                    .str = self.source[start..self.index],
                };
                const ch = self.source[self.index];
                switch (ch) {
                    '=' => {
                        self.index += 1;
                        return .{
                            .kind = .op_lte,
                            .str = self.source[start..self.index],
                        };
                    },
                    '<' => {
                        self.index += 1;
                        return .{
                            .kind = .op_lsh,
                            .str = self.source[start..self.index],
                        };
                    },
                    else => {
                        return .{
                            .kind = .op_lt,
                            .str = self.source[start..self.index],
                        };
                    },
                }
            },
            .greater_than => {
                if (self.index >= self.source.len) return .{
                    .kind = .op_gt,
                    .str = self.source[start..self.index],
                };
                const ch = self.source[self.index];
                switch (ch) {
                    '=' => {
                        self.index += 1;
                        return .{
                            .kind = .op_gte,
                            .str = self.source[start..self.index],
                        };
                    },
                    '>' => {
                        self.index += 1;
                        return .{
                            .kind = .op_rsh,
                            .str = self.source[start..self.index],
                        };
                    },
                    else => {
                        return .{
                            .kind = .op_gt,
                            .str = self.source[start..self.index],
                        };
                    },
                }
            },
            .equals => {
                if (self.index >= self.source.len) return .{
                    .kind = .op_eq,
                    .str = self.source[start..self.index],
                };
                const ch = self.source[self.index];
                switch (ch) {
                    '=' => {
                        self.index += 1;
                        return .{
                            .kind = .op_eq,
                            .str = self.source[start..self.index],
                        };
                    },
                    else => {
                        return .{
                            .kind = .op_assign,
                            .str = self.source[start..self.index],
                        };
                    },
                }
            },
            .bang => {
                if (self.index >= self.source.len) return .{
                    .kind = .invalid_character,
                    .str = self.source[start..self.index],
                };
                const ch = self.source[self.index];
                switch (ch) {
                    '=' => {
                        self.index += 1;
                        return .{
                            .kind = .op_neq,
                            .str = self.source[start..self.index],
                        };
                    },
                    else => {
                        return .{
                            .kind = .invalid_character,
                            .str = self.source[start..self.index],
                        };
                    },
                }
            },
            .open_round => {
                return .{
                    .kind = .open_round,
                    .str = self.source[start..self.index],
                };
            },
            .close_round => {
                return .{
                    .kind = .close_round,
                    .str = self.source[start..self.index],
                };
            },
            .open_block => {
                return .{
                    .kind = .open_block,
                    .str = self.source[start..self.index],
                };
            },
            .close_block => {
                return .{
                    .kind = .close_block,
                    .str = self.source[start..self.index],
                };
            },
            .ampersand => {
                return .{
                    .kind = .op_band,
                    .str = self.source[start..self.index],
                };
            },
            .bar => {
                return .{
                    .kind = .op_bor,
                    .str = self.source[start..self.index],
                };
            },
            .hat => {
                return .{
                    .kind = .op_bxor,
                    .str = self.source[start..self.index],
                };
            },
            .string_literal => {
                if (self.index >= self.source.len) return .{
                    .kind = .string_literal,
                    .str = self.source[start..self.index],
                };
                const ch = self.source[self.index];
                if (ch == '\\') {
                    self.index += 1;
                    continue :next_token .string_escaped;
                } else if (ch == '"') {
                    self.index += 1;
                    return .{
                        .kind = .string_literal,
                        .str = self.source[start + 1 .. self.index - 1],
                    };
                } else {
                    self.index += 1;
                    continue :next_token .string_literal;
                }
            },
            .string_escaped => {
                if (self.index >= self.source.len) return .{
                    .kind = .string_literal,
                    .str = self.source[start..self.index],
                };
                const ch = self.source[self.index];
                switch (ch) {
                    'n', 'r', 't', '\"' => {
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
                    .str = self.source[start..self.index],
                };
            },
            .comma => {
                return .{
                    .kind = .comma,
                    .str = self.source[start..self.index],
                };
            },
            .dot => {
                if (self.index >= self.source.len) return .{
                    .kind = .dot,
                    .str = self.source[start..self.index],
                };
                const ch = self.source[self.index];
                switch (ch) {
                    '{' => {
                        self.index += 1;
                        return .{
                            .kind = .dot_block,
                            .str = self.source[start..self.index],
                        };
                    },
                    else => {
                        return .{
                            .kind = .dot,
                            .str = self.source[start..self.index],
                        };
                    },
                }
            },
            .cr => {
                return .{
                    .kind = .eol,
                    .str = self.source[start..self.index],
                };
            },
            .builtin => {
                if (self.index >= self.source.len) return .{
                    .kind = .identifier,
                    .str = self.source[start..self.index],
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
                            .str = self.source[start..self.index],
                        };
                    },
                }
            },
            .literal_identifier => {
                if (self.index >= self.source.len) return .{
                    .kind = .identifier,
                    .str = self.source[start..self.index],
                };
                const ch = self.source[self.index];
                if (ch == '\\') {
                    self.index += 1;
                    continue :next_token .literal_identifier_escaped;
                } else if (ch == '"') {
                    self.index += 1;
                    return .{
                        .kind = .identifier,
                        .str = self.source[start .. self.index - 1],
                    };
                } else {
                    self.index += 1;
                    continue :next_token .literal_identifier;
                }
            },
            .literal_identifier_escaped => {
                if (self.index >= self.source.len) return .{
                    .kind = .identifier,
                    .str = self.source[start..self.index],
                };
                const ch = self.source[self.index];
                switch (ch) {
                    'n', 'r', 't', '\"' => {
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
            .str = self.source[start..self.index],
        };
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn check_keyword(str: []const u8) Kind {
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

pub const Token = struct {
    kind: Kind,
    str: []const u8,

    pub fn format(self: Token, _: anytype, _: anytype, writer: anytype) !void {
        try writer.print("{s} \"{}\"", .{
            @tagName(self.kind),
            std.zig.fmtEscapes(self.str),
        });
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Kind = enum(u8) {
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

fn test_tokenize(str: []const u8, tokens: []const Token) !void {
    var iter = tokenize(str);
    var index: usize = 0;
    while (iter.next()) |tk| {
        try std.testing.expectEqual(tokens[index].kind, tk.kind);
        try std.testing.expectEqualStrings(tokens[index].str, tk.str);
        index += 1;
    }
    try std.testing.expectEqual(index, tokens.len);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const TestParseOptions = struct {
    trace_tokenize: bool = false,
    trace_parse: bool = false,
};

fn test_parse(str: []const u8, result: []const u8, options: TestParseOptions) !void {
    var moon = Moon.init(std.testing.allocator);
    defer moon.deinit();

    var iter = tokenize(str);
    iter.trace = options.trace_tokenize;

    var tree = moon.AST();
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

    const module = try moon.compile(str, options);

    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();

    const writer = buffer.writer();

    try moon.dump(module, writer);

    try std.testing.expectEqualStrings(result, buffer.items);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize symbols" {
    try test_tokenize("= + - * / % << >> & | ^ ~ == != <= >= < > , . .{", &[_]Token{
        .{ .kind = .op_assign, .str = "=" },
        .{ .kind = .op_add, .str = "+" },
        .{ .kind = .op_sub, .str = "-" },
        .{ .kind = .op_mul, .str = "*" },
        .{ .kind = .op_div, .str = "/" },
        .{ .kind = .op_mod, .str = "%" },
        .{ .kind = .op_lsh, .str = "<<" },
        .{ .kind = .op_rsh, .str = ">>" },
        .{ .kind = .op_band, .str = "&" },
        .{ .kind = .op_bor, .str = "|" },
        .{ .kind = .op_bxor, .str = "^" },
        .{ .kind = .op_com, .str = "~" },
        .{ .kind = .op_eq, .str = "==" },
        .{ .kind = .op_neq, .str = "!=" },
        .{ .kind = .op_lte, .str = "<=" },
        .{ .kind = .op_gte, .str = ">=" },
        .{ .kind = .op_lt, .str = "<" },
        .{ .kind = .op_gt, .str = ">" },
        .{ .kind = .comma, .str = "," },
        .{ .kind = .dot, .str = "." },
        .{ .kind = .dot_block, .str = ".{" },
    });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize identifiers" {
    try test_tokenize("a b a_ _b _ a98", &[_]Token{
        .{ .kind = .identifier, .str = "a" },
        .{ .kind = .identifier, .str = "b" },
        .{ .kind = .identifier, .str = "a_" },
        .{ .kind = .identifier, .str = "_b" },
        .{ .kind = .identifier, .str = "_" },
        .{ .kind = .identifier, .str = "a98" },
    });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize keywords" {
    try test_tokenize(
        \\if elif else not and or while break continue return
        \\@import
        \\true false nil
    , &[_]Token{
        .{ .kind = .keyword_if, .str = "if" },
        .{ .kind = .keyword_elif, .str = "elif" },
        .{ .kind = .keyword_else, .str = "else" },
        .{ .kind = .keyword_not, .str = "not" },
        .{ .kind = .keyword_and, .str = "and" },
        .{ .kind = .keyword_or, .str = "or" },
        .{ .kind = .keyword_while, .str = "while" },
        .{ .kind = .keyword_break, .str = "break" },
        .{ .kind = .keyword_continue, .str = "continue" },
        .{ .kind = .keyword_return, .str = "return" },
        .{ .kind = .eol, .str = "\n" },
        .{ .kind = .keyword_at_import, .str = "@import" },
        .{ .kind = .eol, .str = "\n" },
        .{ .kind = .keyword_true, .str = "true" },
        .{ .kind = .keyword_false, .str = "false" },
        .{ .kind = .keyword_nil, .str = "nil" },
    });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize non-keywords" {
    try test_tokenize("@\"if\"", &[_]Token{
        .{ .kind = .identifier, .str = "if" },
    });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize call" {
    try test_tokenize("print (\"Hello, World!\n\")", &[_]Token{
        .{ .kind = .identifier, .str = "print" },
        .{ .kind = .open_round, .str = "(" },
        .{ .kind = .string_literal, .str = "Hello, World!\n" },
        .{ .kind = .close_round, .str = ")" },
    });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize variables" {
    try test_tokenize("const a = 37\nvar b = 42; var c = a + b", &[_]Token{
        .{ .kind = .keyword_const, .str = "const" },
        .{ .kind = .identifier, .str = "a" },
        .{ .kind = .op_assign, .str = "=" },
        .{ .kind = .integer, .str = "37" },
        .{ .kind = .eol, .str = "\n" },
        .{ .kind = .keyword_var, .str = "var" },
        .{ .kind = .identifier, .str = "b" },
        .{ .kind = .op_assign, .str = "=" },
        .{ .kind = .integer, .str = "42" },
        .{ .kind = .eos, .str = ";" },
        .{ .kind = .keyword_var, .str = "var" },
        .{ .kind = .identifier, .str = "c" },
        .{ .kind = .op_assign, .str = "=" },
        .{ .kind = .identifier, .str = "a" },
        .{ .kind = .op_add, .str = "+" },
        .{ .kind = .identifier, .str = "b" },
    });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize function def" {
    try test_tokenize("pub fn add (a, b) { const c = a + b; return c }", &[_]Token{
        .{ .kind = .keyword_pub, .str = "pub" },
        .{ .kind = .keyword_fn, .str = "fn" },
        .{ .kind = .identifier, .str = "add" },
        .{ .kind = .open_round, .str = "(" },
        .{ .kind = .identifier, .str = "a" },
        .{ .kind = .comma, .str = "," },
        .{ .kind = .identifier, .str = "b" },
        .{ .kind = .close_round, .str = ")" },
        .{ .kind = .open_block, .str = "{" },
        .{ .kind = .keyword_const, .str = "const" },
        .{ .kind = .identifier, .str = "c" },
        .{ .kind = .op_assign, .str = "=" },
        .{ .kind = .identifier, .str = "a" },
        .{ .kind = .op_add, .str = "+" },
        .{ .kind = .identifier, .str = "b" },
        .{ .kind = .eos, .str = ";" },
        .{ .kind = .keyword_return, .str = "return" },
        .{ .kind = .identifier, .str = "c" },
        .{ .kind = .close_block, .str = "}" },
    });
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
        \\ 37 + 42 * (12 - 2) / 3
        \\
    ,
        \\AST
        \\  block
        \\    op_add
        \\      integer_literal 37
        \\      op_div
        \\        op_mul
        \\          integer_literal 42
        \\          op_sub
        \\            integer_literal 12
        \\            integer_literal 2
        \\        integer_literal 3
        \\
    , .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "parse operator precedence" {
    try test_parse(
        \\ (0x30 << 3) >> 2 & 0xFF | 0x10
        \\
    ,
        \\AST
        \\  block
        \\    op_bor
        \\      op_band
        \\        op_rsh
        \\          op_lsh
        \\            integer_literal 48
        \\            integer_literal 3
        \\          integer_literal 2
        \\        integer_literal 255
        \\      integer_literal 16
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
        \\    func_decl add
        \\      a
        \\      b
        \\      block
        \\        return_stmt
        \\          op_add
        \\            identifier a
        \\            identifier b
        \\    func_decl eol
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

test "compile simple" {
    try test_compile(
        \\ fn add (a, b)
        \\ {
        \\     return a + b
        \\ }
        \\
        \\ add (37, 42)
        \\
    ,
        \\module
        \\  code
        \\    000000 load_global 0 ; "add"
        \\    000001 integer 37
        \\    000002 integer 42
        \\    000003 call 2
        \\    000004 drop 1
        \\    000005 ret
        \\    000006 load_local 0
        \\    000007 load_local 1
        \\    000008 add
        \\    000009 ret
        \\  constants
        \\  globals
        \\    0: "add" function 000006
        \\  strings
        \\
    ,
        .{},
    );
}

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

test "compile hello world" {
    try test_compile(
        \\ const std = @import ("std");
        \\ std.print ("Hello, World!\n", .{37, 42})
        \\
    ,
        \\module
        \\  globals
        \\    std
        \\  constants
        \\    0: "std"
        \\    1: "print"
        \\    2: "Hello, World!\n"
        \\  code
        \\    push_constant 0
        \\    call_import 1
        \\    store_global std
        \\    push 2
        \\    new_table
        \\    load_global std
        \\    push_constant 1 // "print"
        \\    load_table
        \\    call 2
        \\
    ,
        .{
            .trace_code = true,
        },
    );
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
