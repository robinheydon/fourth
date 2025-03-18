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

pub var button_clicks: usize = 0;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub var window: ng.Window = undefined;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub var dt: f32 = 0;

pub var map_move_velocity: ng.Vec2 = .{ 0, 0 };
pub var map_center: ng.Vec2 = .{ 0, 0 };
pub var map_zoom: f32 = 10;
pub var map_rotate: f32 = 0;

pub var camera: ng.Camera2D = .{};

pub var map_last_mouse: ng.Vec2 = .{ 0, 0 };
pub var map_start_click: ng.Vec2 = .{ 0, 0 };
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

pub var show_debug_info = true;
pub var show_window2 = false;
pub var show_window3 = false;
pub var show_window4 = false;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
