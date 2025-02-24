///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import ("std");

var min_level : Level = .debug;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn log_print (comptime level: Level, comptime scope: @Type(.enum_literal), comptime format: []const u8, args: anytype,) void
{
    if (@intFromEnum (level) >= @intFromEnum (min_level))
    {
        std.debug.print (level.string () ++ "{s: <8}" ++ format ++ "\n", .{ @tagName (scope) } ++ args);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn Logger (comptime scope: @Type(.enum_literal)) type {
    return struct {
        pub fn note (comptime format: []const u8, args: anytype) void
        {
            log_print (.note, scope, format, args);
        }
        pub fn info (comptime format: []const u8, args: anytype) void
        {
            log_print (.info, scope, format, args);
        }
        pub fn msg (comptime format: []const u8, args: anytype) void
        {
            log_print (.msg, scope, format, args);
        }
        pub fn debug (comptime format: []const u8, args: anytype) void
        {
            log_print (.debug, scope, format, args);
        }
        pub fn warn (comptime format: []const u8, args: anytype) void
        {
            log_print (.warn, scope, format, args);
        }
        pub fn err (comptime format: []const u8, args: anytype) void
        {
            log_print (.err, scope, format, args);
        }
        pub fn fatal (comptime format: []const u8, args: anytype) void
        {
            log_print (.fatal, scope, format, args);
        }
        pub fn print (comptime level: Level, comptime format: []const u8, args: anytype) void
        {
            log_print (level, scope, format, args);
        }
        pub fn set_min_level (level: Level) void {
            min_level = level;
        }
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Level = enum {
    note,
    info,
    msg,
    debug,
    warn,
    err,
    fatal,

    pub fn string (self: Level) []const u8 {
        return switch (self) {
            .note  => "note    ",
            .info  => "info    ",
            .debug => "debug   ",
            .msg   => "message ",
            .warn  => "Warning ",
            .err   => "ERROR   ",
            .fatal => "FATAL   ",
        };
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
