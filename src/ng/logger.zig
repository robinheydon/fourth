///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

const time = @import("time.zig");

var min_level: Level = .debug;
var depth: usize = 0;
const lots_of_spaces = "  " ** 256;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn log_print(
    comptime level: Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    if (@intFromEnum(level) >= @intFromEnum(min_level)) {
        if (time.initialized) {
            std.debug.print(
                level.string() ++ "{s: <8}{d:12.6} | {s}" ++ format ++ "\n",
                .{ @tagName(scope), time.elapsed(), lots_of_spaces[0 .. depth * 2] } ++ args,
            );
        } else {
            std.debug.print(
                level.string() ++ "{s: <8} | {s}" ++ format ++ "\n",
                .{ @tagName(scope), lots_of_spaces[0 .. depth * 2] } ++ args,
            );
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn Logger(comptime scope: @Type(.enum_literal)) type {
    return struct {
        pub fn note(comptime format: []const u8, args: anytype) void {
            log_print(.note, scope, format, args);
        }
        pub fn info(comptime format: []const u8, args: anytype) void {
            log_print(.info, scope, format, args);
        }
        pub fn msg(comptime format: []const u8, args: anytype) void {
            log_print(.msg, scope, format, args);
        }
        pub fn debug(comptime format: []const u8, args: anytype) void {
            log_print(.debug, scope, format, args);
        }
        pub fn warn(comptime format: []const u8, args: anytype) void {
            log_print(.warn, scope, format, args);
        }
        pub fn err(comptime format: []const u8, args: anytype) void {
            log_print(.err, scope, format, args);
        }
        pub fn fatal(comptime format: []const u8, args: anytype) noreturn {
            log_print(.fatal, scope, format, args);
            @panic("Fatal log error");
        }
        pub fn set_min_level(level: Level) void {
            min_level = level;
        }
        pub fn inc_depth() void {
            depth += 1;
        }
        pub fn dec_depth() void {
            depth -= 1;
        }
        pub fn callstack() void {
            std.debug.dumpCurrentStackTrace(null);
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

    pub fn string(self: Level) []const u8 {
        return switch (self) {
            .note => "note    ",
            .info => "info    ",
            .debug => "debug   ",
            .msg => "msg     ",
            .warn => "Warning ",
            .err => "ERROR   ",
            .fatal => "FATAL   ",
        };
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
