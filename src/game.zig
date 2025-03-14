const std = @import ("std");

const State = struct
{
    frame_count: usize = 0,
};

var the_state: *State = undefined;

pub export fn init () void
{
    std.debug.print ("init\n", .{});
}

pub export fn deinit () void
{
    std.debug.print ("deinit\n", .{});
}

pub export fn update () void
{
    std.debug.print ("update\n", .{});
}
