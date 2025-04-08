///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Moon = struct {
    allocator: std.mem.Allocator,

    globals: SymbolTable,

    stack: std.ArrayListUnmanaged(Value),

    pub fn init(alloc: std.mem.Allocator) Moon {
        return .{
            .allocator = alloc,
            .globals = .empty,
            .stack = .empty,
        };
    }

    pub fn deinit(self: *Moon) void {
        self.stack.deinit(self.allocator);
        self.globals.deinit(self.allocator);
    }

    pub fn push_integer(self: *Moon, value: i64) MoonErrors!void {
        try self.stack.append(self.allocator, .{ .integer = value });
    }

    pub fn op(self: *Moon, operation: Operation) MoonErrors!void {
        switch (operation) {
            .add => {
                if (self.stack.pop()) |rhs| {
                    if (self.stack.pop()) |lhs| {
                        const res = try Value.add(lhs, rhs);
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

    pub fn compile(self: *Moon, source: []const u8) MoonErrors!Value {
        _ = self;

        var token_iter = tokenize(source);
        while (token_iter.next()) |token| {
            std.debug.print("Token {}\n", .{token});
        }
        return .{ .nil = {} };
    }

    pub fn AST(self: *Moon) Moon_AST {
        return .{
            .moon = self,
        };
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
                .statements => |*stmts| {
                    self.moon.allocator.free(stmts.items);
                },
                .call => |*call| {
                    self.moon.allocator.free(call.args);
                },
                else => {},
            }
        }
        self.nodes.deinit(self.moon.allocator);
    }

    pub fn parse(self: *Moon_AST, iter: *TokenIterator) MoonErrors!usize {
        return try self.parse_statements(iter);
    }

    pub fn parse_statements(self: *Moon_AST, iter: *TokenIterator) MoonErrors!usize {
        var stmts: std.ArrayListUnmanaged(usize) = .empty;
        errdefer stmts.deinit(self.moon.allocator);

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
                    .eol => {
                        _ = iter.next();
                    },
                    .eos => {
                        _ = iter.next();
                    },
                    .close_block => {
                        break;
                    },
                    else => {
                        return error.InvalidStatement;
                    },
                }
            }
        }

        return try self.new_node(.{ .statements = .{
            .items = try stmts.toOwnedSlice(self.moon.allocator),
        } });
    }

    pub fn parse_var_decl(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!usize {
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
    ) MoonErrors!usize {
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
    ) MoonErrors!usize {
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

    pub fn parse_block(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!usize {
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
    ) MoonErrors!usize {
        if (self.trace) {
            const tk = iter.peek();
            std.debug.print("parse_expr: {?}\n", .{tk});
        }

        return try self.parse_logical_or(iter);
    }

    pub fn parse_logical_or(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors!usize {
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
    ) MoonErrors!usize {
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
    ) MoonErrors!usize {
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
    ) MoonErrors!usize {
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
    ) MoonErrors!usize {
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

    pub fn parse_unary(self: *Moon_AST, iter: *TokenIterator) MoonErrors!usize {
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

    pub fn parse_atom(self: *Moon_AST, iter: *TokenIterator) MoonErrors!usize {
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
                .string_literal => {
                    _ = iter.next();
                    return try self.new_node(
                        .{ .string = tk.str },
                    );
                },
                .eol => {
                    _ = iter.next();
                    return self.parse_atom(iter);
                },
                else => {
                    std.debug.print("Invalid Expression at {}\n", .{tk});
                    return error.InvalidExpressionAtom;
                },
            }
        }

        return error.InvalidExpression;
    }

    pub fn parse_prefix(self: *Moon_AST, iter: *TokenIterator) MoonErrors!usize {
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
                        _ = iter.next ();
                        if (iter.peek ()) |itk|
                        {
                            if (itk.kind == .identifier)
                            {
                                const rhs = try self.parse_prefix (iter);
                                lhs = try self.new_node (.{
                                    .op_dot = .{
                                        .lhs = lhs,
                                        .rhs = rhs,
                                    }});
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
                    }
                }
            }

            return lhs;
        }
        return error.MissingIdentifier;
    }

    pub fn parse_args(
        self: *Moon_AST,
        iter: *TokenIterator,
    ) MoonErrors![]usize {
        var args: std.ArrayListUnmanaged(usize) = .empty;
        errdefer args.deinit(self.moon.allocator);

        while (iter.peek()) |tk| {
            if (self.trace) {
                std.debug.print("parse_args: {}\n", .{tk});
            }
            if (tk.kind == .eol) {
                _ = iter.next();
                continue;
            }
            if (is_expression(tk.kind)) {
                const arg = try self.parse_expr(iter);
                try args.append(self.moon.allocator, arg);

                while (iter.peek()) |ctk| {
                    if (ctk.kind == .eol) {
                        _ = iter.next();
                    } else {
                        break;
                    }
                }

                if (iter.peek()) |ctk| {
                    if (ctk.kind == .comma) {
                        _ = iter.next();
                    }
                }

                while (iter.peek()) |ctk| {
                    if (ctk.kind == .eol) {
                        _ = iter.next();
                    } else {
                        break;
                    }
                }

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
            .keyword_true,
            .keyword_false,
            => return true,
            else => return false,
        }
    }

    pub fn new_node(self: *Moon_AST, node: AST_Node) MoonErrors!usize {
        const index = self.nodes.items.len;
        try self.nodes.append(self.moon.allocator, node);
        return index;
    }

    pub fn update_node(self: *Moon_AST, index: usize, node: AST_Node) void {
        std.debug.assert(index < self.nodes.items.len);
        self.nodes.items[index] = node;
    }

    pub fn dump(self: *Moon_AST, index: usize, writer: anytype) !void {
        try writer.print("AST\n", .{});
        try self.dump_internal(index, 1, writer);
    }

    const lots_of_spaces = " " ** 256;

    fn dump_internal(self: *Moon_AST, index: usize, depth: usize, writer: anytype) !void {
        if (index < self.nodes.items.len) {
            try writer.print("{s}", .{lots_of_spaces[0 .. depth * 2]});
            const node = self.nodes.items[index];
            try writer.print("{s}", .{@tagName(node)});
            switch (node) {
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
                    try self.dump_internal(op.lhs, depth + 1, writer);
                    try self.dump_internal(op.rhs, depth + 1, writer);
                },
                .op_not,
                .op_neg,
                .op_com,
                => |op| {
                    try writer.print("\n", .{});
                    try self.dump_internal(op, depth + 1, writer);
                },
                .call => |call| {
                    try writer.print("\n", .{});
                    try self.dump_internal(call.func, depth + 1, writer);
                    for (call.args) |item| {
                        try self.dump_internal(item, depth + 1, writer);
                    }
                },
                .statements => |stmts| {
                    try writer.print("\n", .{});
                    for (stmts.items) |item| {
                        try self.dump_internal(item, depth + 1, writer);
                    }
                },
                .var_decl => |decl| {
                    try writer.print(" {s}\n", .{decl.name});
                    try self.dump_internal(decl.expr, depth + 1, writer);
                },
                .const_decl => |decl| {
                    try writer.print(" {s}\n", .{decl.name});
                    try self.dump_internal(decl.expr, depth + 1, writer);
                },
                .while_stmt => |stmt| {
                    try writer.print("\n", .{});
                    try self.dump_internal(stmt.expr, depth + 1, writer);
                    try self.dump_internal(stmt.block, depth + 1, writer);
                },
                .assignment => |stmt| {
                    try writer.print("\n", .{});
                    try self.dump_internal(stmt.lhs, depth + 1, writer);
                    try self.dump_internal(stmt.rhs, depth + 1, writer);
                },
            }
        }
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const AST_NodeKind = enum {
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
    statements,
    var_decl,
    const_decl,
    while_stmt,
    assignment,
};

pub const AST_Node = union(AST_NodeKind) {
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
    op_not: usize,
    op_neg: usize,
    op_com: usize,
    call: Call,
    statements: AST_List,
    var_decl: Decl,
    const_decl: Decl,
    while_stmt: WhileStmt,
    assignment: AST_BinaryOp,
};

pub const AST_BinaryOp = struct {
    lhs: usize,
    rhs: usize,
};

pub const AST_List = struct {
    items: []usize,
};

pub const Call = struct {
    func: usize,
    args: []usize,
};

pub const Decl = struct {
    name: []const u8,
    expr: usize,
};

pub const WhileStmt = struct {
    expr: usize,
    block: usize,
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
    MissingCloseParenthesis,
    MissingStatements,
    MissingIdentifier,
    MissingAssignment,
    MissingOpenBlock,
    MissingCloseBlock,
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

    pub fn add(lhs: Value, rhs: Value) MoonErrors!Value {
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

pub const SymbolTable = std.AutoHashMapUnmanaged(StringIndex, Value);
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
                        .str = self.source[start..self.index],
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
                if (ch == 'n') {
                    self.index += 1;
                    continue :next_token .string_literal;
                } else {
                    continue :next_token .string_literal;
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
                return .{
                    .kind = .dot,
                    .str = self.source[start..self.index],
                };
            },
            .cr => {
                return .{
                    .kind = .eol,
                    .str = self.source[start..self.index],
                };
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
            if (std.mem.eql(u8, "not", str)) return .keyword_not;
            if (std.mem.eql(u8, "pub", str)) return .keyword_pub;
            if (std.mem.eql(u8, "var", str)) return .keyword_var;
        },
        4 => {
            if (std.mem.eql(u8, "else", str)) return .keyword_else;
            if (std.mem.eql(u8, "then", str)) return .keyword_then;
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
    comma,
    keyword_if,
    keyword_then,
    keyword_else,
    keyword_while,
    keyword_break,
    keyword_not,
    keyword_and,
    keyword_or,
    keyword_continue,
    keyword_return,
    keyword_const,
    keyword_var,
    keyword_pub,
    keyword_fn,
    keyword_true,
    keyword_false,
    eol,
    eos,
};

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
        try std.testing.expectEqual(tk.kind, tokens[index].kind);
        try std.testing.expectEqualStrings(tk.str, tokens[index].str);
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

    try tree.dump(root, writer);

    try std.testing.expectEqualStrings(result, buffer.items);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize symbols" {
    try test_tokenize("= + - * / % << >> & | ^ ~ == != <= >= < > , .", &[_]Token{
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
    try test_tokenize("if then else not and or while break continue return", &[_]Token{
        .{ .kind = .keyword_if, .str = "if" },
        .{ .kind = .keyword_then, .str = "then" },
        .{ .kind = .keyword_else, .str = "else" },
        .{ .kind = .keyword_not, .str = "not" },
        .{ .kind = .keyword_and, .str = "and" },
        .{ .kind = .keyword_or, .str = "or" },
        .{ .kind = .keyword_while, .str = "while" },
        .{ .kind = .keyword_break, .str = "break" },
        .{ .kind = .keyword_continue, .str = "continue" },
        .{ .kind = .keyword_return, .str = "return" },
    });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize call" {
    try test_tokenize("print (\"Hello, World!\n\")", &[_]Token{
        .{ .kind = .identifier, .str = "print" },
        .{ .kind = .open_round, .str = "(" },
        .{ .kind = .string_literal, .str = "\"Hello, World!\n\"" },
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
        \\  statements
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
        \\  statements
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
        \\  statements
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
        \\  statements
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
        \\  statements
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
        \\  statements
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
        \\  statements
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
        \\  statements
        \\    var_decl a
        \\      integer_literal 0
        \\    while_stmt
        \\      op_lt
        \\        identifier a
        \\        integer_literal 4
        \\      statements
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
        \\  statements
        \\    call
        \\      identifier print
        \\      string "\"Hello, World!\\n\""
        \\      integer_literal 32
        \\      op_neg
        \\        integer_literal 33
        \\      op_com
        \\        integer_literal 34
        \\      boolean_true
        \\      boolean_false
        \\      op_not
        \\        boolean_true
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
        \\  statements
        \\    const_decl dist
        \\      call
        \\        op_dot
        \\          op_dot
        \\            identifier std
        \\            identifier math
        \\          identifier sqrt
        \\        op_add
        \\          op_mul
        \\            op_dot
        \\              identifier pos
        \\              identifier x
        \\            op_dot
        \\              identifier pos
        \\              identifier x
        \\          op_mul
        \\            op_dot
        \\              identifier pos
        \\              identifier y
        \\            op_dot
        \\              identifier pos
        \\              identifier y
        \\
    , .{
        .trace_tokenize = true,
        .trace_parse = true,
    });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
