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
};

pub const MoonErrors = error{
    NotImplemented,
    NotInteger,
    OutOfMemory,
    StackUnderflow,
    TypeMismatch,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Kind = enum {
    nil,
    integer,
    number,
    string,
    function,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Value = union(Kind) {
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

    const State = enum {
        whitespace,
        identifier,
        integer,
        number,
        zero,
        plus,
        minus,
        star,
        slash,
        percent,
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
        semicolon,
    };

    pub fn peek(self: *TokenIterator) ?Token {
        const old_index = self.index;
        const tk = self.next();
        self.index = old_index;
        return tk;
    }

    pub fn next(self: *TokenIterator) ?Token {
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
                return .{
                    .kind = .integer,
                    .str = self.source[start..self.index],
                };
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

fn check_keyword(str: []const u8) TokenKind {
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
        },
        5 => {
            if (std.mem.eql(u8, "break", str)) return .keyword_break;
            if (std.mem.eql(u8, "const", str)) return .keyword_const;
            if (std.mem.eql(u8, "while", str)) return .keyword_while;
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
    kind: TokenKind,
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
    op_band,
    op_bor,
    op_bxor,
    op_eq,
    op_neq,
    op_lte,
    op_gte,
    op_lt,
    op_gt,
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

test "tokenize operators" {
    var moon = Moon.init(std.testing.allocator);
    defer moon.deinit();

    try test_tokenize("= + - * / % << >> & | ^ == != <= >= < >", &[_]Token{
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
        .{ .kind = .op_eq, .str = "==" },
        .{ .kind = .op_neq, .str = "!=" },
        .{ .kind = .op_lte, .str = "<=" },
        .{ .kind = .op_gte, .str = ">=" },
        .{ .kind = .op_lt, .str = "<" },
        .{ .kind = .op_gt, .str = ">" },
    });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "tokenize identifiers" {
    var moon = Moon.init(std.testing.allocator);
    defer moon.deinit();

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
    var moon = Moon.init(std.testing.allocator);
    defer moon.deinit();

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
    var moon = Moon.init(std.testing.allocator);
    defer moon.deinit();

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
    var moon = Moon.init(std.testing.allocator);
    defer moon.deinit();

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
    var moon = Moon.init(std.testing.allocator);
    defer moon.deinit();

    try test_tokenize("pub fn add (a, b) { return a + b }", &[_]Token{
        .{ .kind = .keyword_pub, .str = "pub" },
        .{ .kind = .keyword_fn, .str = "fn" },
        .{ .kind = .identifier, .str = "add" },
        .{ .kind = .open_round, .str = "(" },
        .{ .kind = .identifier, .str = "a" },
        .{ .kind = .comma, .str = "," },
        .{ .kind = .identifier, .str = "b" },
        .{ .kind = .close_round, .str = ")" },
        .{ .kind = .open_block, .str = "{" },
        .{ .kind = .keyword_return, .str = "return" },
        .{ .kind = .identifier, .str = "a" },
        .{ .kind = .op_add, .str = "+" },
        .{ .kind = .identifier, .str = "b" },
        .{ .kind = .close_block, .str = "}" },
    });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
