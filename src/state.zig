///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const ng = @import("ng");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub var running = true;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub var frame_counter: usize = 0;
pub var average_frame_rate: u32 = 0;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub var window: ng.Window = undefined;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub var dt: f32 = 0;

pub var map_move_velocity: ng.Vec2 = .{ 0, 0 };
pub var map_center: ng.Vec2 = .{ 10, 10 };
pub var map_zoom: f32 = 10;
pub var map_rotate: f32 = 0;

pub var camera: ng.Camera2D = .{};

pub var map_last_mouse: ng.Vec2 = .{ 0, 0 };
pub var map_start_click_x: f32 = 0;
pub var map_start_click_y: f32 = 0;
pub var map_state: MapState = .none;

pub var key_quit: ng.Key = .escape;
pub var key_toggle_fullscreen: ng.Key = .f11;
pub var key_move_up: ng.Key = .W;
pub var key_move_down: ng.Key = .S;
pub var key_move_left: ng.Key = .A;
pub var key_move_right: ng.Key = .D;
pub var key_move_up2: ng.Key = .up;
pub var key_move_down2: ng.Key = .down;
pub var key_move_left2: ng.Key = .left;
pub var key_move_right2: ng.Key = .right;
pub var key_rotate_left: ng.Key = .Q;
pub var key_rotate_right: ng.Key = .E;
pub var key_toggle_debug_info: ng.Key = .f1;
pub var key_toggle_window2: ng.Key = .f2;
pub var key_toggle_window3: ng.Key = .f3;
pub var key_toggle_window4: ng.Key = .f4;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const axis_shader_source = @import("triangle_shader.zig");
pub const AxisVertex = axis_shader_source.Vertex;
pub const AxisUniforms = ng.make_uniform_slots(axis_shader_source.Uniforms);

pub var axis_shader: ng.Shader = undefined;
pub var axis_buffer: ng.Buffer = undefined;
pub var axis_pipeline: ng.Pipeline = undefined;
pub var axis_binding: ng.Binding = undefined;

pub const axis_data = [_]AxisVertex{
    .{ .pos = .{ 1, 0 }, .col = .green },
    .{ .pos = .{ 1, 1 }, .col = .green },
    .{ .pos = .{ 20, 0 }, .col = .green },
    .{ .pos = .{ 20, 0 }, .col = .green },
    .{ .pos = .{ 1, 1 }, .col = .green },
    .{ .pos = .{ 20, 1 }, .col = .green },

    .{ .pos = .{ 0, 1 }, .col = .red },
    .{ .pos = .{ 1, 1 }, .col = .red },
    .{ .pos = .{ 0, 20 }, .col = .red },
    .{ .pos = .{ 0, 20 }, .col = .red },
    .{ .pos = .{ 1, 1 }, .col = .red },
    .{ .pos = .{ 1, 20 }, .col = .red },
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const grid_shader_source = @import("grid_shader.zig");
pub const GridUniforms = ng.make_uniform_slots(grid_shader_source.Uniforms);

pub var grid_shader: ng.Shader = undefined;
pub var grid_pipeline: ng.Pipeline = undefined;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const MapState = enum {
    none,
    clicked,
    dragging,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Node = extern struct {
    pos: ng.Vec2,
};

pub const Link = extern struct {
    start: ng.Entity, // start of the link
    mid: ng.Entity, // a mid point of the link, used for curves
    end: ng.Entity, // the end of the link
    width: u16, // the width of the link in 0.1m
};

pub const Construction = extern struct {
    step: f32,
    steps: f32,
};

pub const Health = extern struct {
    hp: u8,
    max: u8,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub var show_debug_info = false;
pub var show_window2 = false;
pub var show_window3 = false;
pub var show_window4 = false;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
