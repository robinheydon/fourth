///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

const ng = @import("ng");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const state = @import("state.zig");

pub const log = ng.Logger(.main);

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn main() !void {
    log.set_min_level(.note);
    log.info("starting", .{});
    defer log.info("ending", .{});

    try ng.init(.{
        .video = true,
        .audio = true,
    });
    defer ng.deinit();

    state.window = try ng.create_window(.{
        .name = "Fourth",
        .width = 1280,
        .height = 720,
        .resizable = true,
    });
    defer state.window.close();

    state.window.set_swap_interval(.lowpower);

    init_world();

    try init_draw_world();

    while (state.running) {
        state.frame_counter +%= 1;

        state.dt = ng.start_frame();
        defer ng.end_frame();

        ng.debug_clear(state.window);

        update_fps(state.dt);

        if (state.average_frame_rate > 0) {
            ng.debug_print("{} Hz\n{} frames\n", .{
                state.average_frame_rate,
                state.frame_counter,
            });
        }

        process_events();

        draw_ui();

        const command_buffer = try state.window.acquire_command_buffer();
        const swapchain_texture = try command_buffer.acquire_swapchain_texture();
        const render_pass = try command_buffer.begin_render_pass(.{
            .texture = swapchain_texture,
            .clear_color = .@"dark grey",
            .load = .clear,
            .store = .store,
        });

        draw_world(render_pass);

        ng.ui_render(render_pass);

        ng.debug_text_draw(render_pass);

        render_pass.end();
        try command_buffer.submit();
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn draw_ui() void {
    if (state.show_debug_info) {
        draw_debug_window();
    }
    if (state.show_window2) {
        draw_window2();
    }
    if (state.show_window3) {
        draw_window3();
    }
    if (state.show_window4) {
        draw_window4();
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn draw_debug_window() void {
    ng.ui_begin_window(.{
        .title = "Debug Window",
        .x = 80,
        .y = 100,
        .width = 320,
        .height = 200,
        .background_color = .black,
    });
    defer ng.ui_end_window();

    // ng.ui_begin_box(.{ .direction = .vertical });
    // ng.ui_format_text(.{}, "{} fps", .{state.average_frame_rate});
    // ng.ui_end_box();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn draw_window2() void {
    ng.ui_begin_window(.{
        .title = "Window2",
        .x = 160,
        .y = 120,
        .width = 320,
        .height = 200,
        .background_color = .red,
    });
    defer ng.ui_end_window();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn draw_window3() void {
    ng.ui_begin_window(.{
        .title = "Window3",
        .x = 240,
        .y = 140,
        .width = 320,
        .height = 200,
        .background_color = .green,
    });
    defer ng.ui_end_window();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn draw_window4() void {
    ng.ui_begin_window(.{
        .title = "Window4",
        .x = 320,
        .y = 160,
        .width = 320,
        .height = 200,
        .background_color = .blue,
    });
    defer ng.ui_end_window();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn init_draw_world() !void {
    state.axis_shader = try ng.create_shader(state.axis_shader_source);

    state.grid_shader = try ng.create_shader(state.grid_shader_source);

    state.grid_pipeline = try ng.create_pipeline(.{
        .label = "Grid Pipeline",
        .shader = state.grid_shader,
        .primitive = .triangle_list,
        .blend = .{
            .enabled = true,
            .src_factor_rgb = .src_alpha,
            .dst_factor_rgb = .one_minus_src_alpha,
            .src_factor_alpha = .one,
            .dst_factor_alpha = .zero,
        },
    });

    // green - x-axis
    // red - y-axis
    state.axis_buffer = try ng.create_buffer(.{
        .label = "Axis Vertex Data",
        .data = ng.as_bytes(&state.axis_data),
    });

    state.axis_pipeline = try ng.create_pipeline(.{
        .label = "Axis Pipeline",
        .shader = state.axis_shader,
        .primitive = .triangle_list,
    });

    state.axis_binding = try ng.create_binding(.{
        .label = "Axis Bindings",
        .vertex_buffers = &.{state.axis_buffer},
    });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn draw_world(render_pass: ng.RenderPass) void {
    const window_size = state.window.get_size();
    const projection = ng.ortho(window_size);

    state.camera = .identity();
    state.camera.zoom = state.map_zoom;
    state.camera.rotate = state.map_rotate;
    state.camera.origin = state.map_center;
    state.camera.target = window_size / ng.Vec2 {2, 2};
    const view = state.camera.get_matrix();
    const mvp = ng.mat4_mul(view, projection);

    debug_map_state();

    render_pass.apply_pipeline(state.grid_pipeline);
    render_pass.apply_uniform_mat4(state.GridUniforms.mvp, mvp);
    render_pass.draw(6);

    render_pass.apply_pipeline(state.axis_pipeline);
    render_pass.apply_bindings(state.axis_binding);
    render_pass.apply_uniform_mat4(state.AxisUniforms.mvp, mvp);
    render_pass.draw(state.axis_data.len);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_events() void {
    while (ng.poll_event()) |event| {
        switch (event) {
            .quit => {
                state.running = false;
            },
            .key_down => |key_event| {
                process_key_down(key_event);
            },
            .key_up => {},
            .mouse_move => |move_event| {
                process_mouse_move(move_event);
            },
            .mouse_double_click => |mouse_event| {
                process_mouse_double_click(mouse_event);
            },
            .mouse_down => |mouse_event| {
                process_mouse_down(mouse_event);
            },
            .mouse_up => |mouse_event| {
                process_mouse_up(mouse_event);
            },
            .mouse_wheel => |wheel_event| {
                process_wheel_event(wheel_event);
            },
            .resize => {
                state.window.acknowledge_resize();
            },
            .enter => {},
            .leave => {},
            .focus => {},
            .unfocus => {},
            else => {
                log.debug("{}", .{event});
            },
        }
    }

    const move_speed = 400 / state.map_zoom;

    if (ng.is_key_down(state.key_move_left) or ng.is_key_down(state.key_move_left2)) {
        if (state.map_move_velocity[0] > 0) {
            state.map_move_velocity[0] = 0;
        }
        state.map_move_velocity[0] -= state.dt * move_speed;
    } else if (ng.is_key_down(state.key_move_right) or ng.is_key_down(state.key_move_right2)) {
        if (state.map_move_velocity[0] < 0) {
            state.map_move_velocity[0] = 0;
        }
        state.map_move_velocity[0] += state.dt * move_speed;
    } else {
        state.map_move_velocity[0] -= state.map_move_velocity[0] * 10 * state.dt;
        if (@abs(state.map_move_velocity[0]) < 0.1) {
            state.map_move_velocity[0] = 0;
        }
    }
    if (ng.is_key_down(state.key_move_down) or ng.is_key_down(state.key_move_down2)) {
        if (state.map_move_velocity[1] > 0) {
            state.map_move_velocity[1] = 0;
        }
        state.map_move_velocity[1] -= state.dt * move_speed;
    } else if (ng.is_key_down(state.key_move_up) or ng.is_key_down(state.key_move_up2)) {
        if (state.map_move_velocity[1] < 0) {
            state.map_move_velocity[1] = 0;
        }
        state.map_move_velocity[1] += state.dt * move_speed;
    } else {
        state.map_move_velocity[1] -= state.map_move_velocity[1] * 10 * state.dt;
        if (@abs(state.map_move_velocity[1]) < 0.1) {
            state.map_move_velocity[1] = 0;
        }
    }

    if (ng.is_key_down(state.key_rotate_left)) {
        state.map_rotate += state.dt;
    } else if (ng.is_key_down(state.key_rotate_right)) {
        state.map_rotate -= state.dt;
    }

    state.map_move_velocity[0] = std.math.clamp(
        state.map_move_velocity[0],
        -move_speed,
        move_speed,
    );
    state.map_move_velocity[1] = std.math.clamp(
        state.map_move_velocity[1],
        -move_speed,
        move_speed,
    );

    state.map_center += state.map_move_velocity * ng.Vec2{ state.dt, state.dt };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_key_down(event: ng.KeyEvent) void {
    if (event.key == state.key_quit) {
        state.running = false;
    } else if (event.key == state.key_toggle_fullscreen) {
        state.window.toggle_fullscreen();
    } else if (event.key == state.key_toggle_debug_info) {
        state.show_debug_info = !state.show_debug_info;
    } else if (event.key == state.key_toggle_window2) {
        state.show_window2 = !state.show_window2;
    } else if (event.key == state.key_toggle_window3) {
        state.show_window3 = !state.show_window3;
    } else if (event.key == state.key_toggle_window4) {
        state.show_window4 = !state.show_window4;
    } else {
        // log.debug("{} ({})", .{ event.key, event.scan_code });
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_mouse_move(event: ng.MoveEvent) void {
    if (state.map_state == .clicked) {
        const delta = @abs(state.map_start_click - event.pos);

        if (delta[0] > 8 or delta[1] > 8) {
            state.map_state = .dragging;
        }
    }

    if (state.map_state == .dragging) {
        const start_position = state.camera.to_world(state.map_last_mouse);
        const end_position = state.camera.to_world(event.pos);
        const delta_pos = start_position - end_position;

        state.map_center += delta_pos;
    }

    state.map_last_mouse = event.pos;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_mouse_double_click(event: ng.MouseEvent) void {
    _ = event; // log.info("double click {}", .{event.button});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_mouse_down(event: ng.MouseEvent) void {
    if (event.button == .left) {
        state.map_state = .clicked;
        state.map_start_click = event.pos;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_mouse_up(event: ng.MouseEvent) void {
    if (event.button == .left) {
        state.map_state = .none;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_wheel_event(event: ng.WheelEvent) void {
    const min_zoom = 0.1; // a few 10km squares
    const max_zoom = 100; // a few 1m squares

    const multiplier = std.math.pow(f32, 1.2, event.dy);
    state.map_zoom = std.math.clamp(state.map_zoom * multiplier, min_zoom, max_zoom);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

var all_delta_times: [65536]f32 = undefined;
var next_dt_index: usize = 0;

fn update_fps(dt: f32) void {
    all_delta_times[next_dt_index] = dt;
    next_dt_index = next_dt_index + 1;
    var total_ft: f32 = 0;
    for (all_delta_times[0..next_dt_index]) |ft| {
        total_ft += ft;
    }
    if (total_ft >= 1.0 or next_dt_index == all_delta_times.len) {
        state.average_frame_rate = @intCast(next_dt_index);
        next_dt_index = 0;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn debug_map_state() void {
    // const position = state.camera.to_world(state.map_last_mouse);
    // ng.debug_print("{d:8.5} -> {d:8.5}\n", .{ state.map_last_mouse, position });

    ng.debug_print("{d:8.5} {d:8.5}\n", .{ state.map_move_velocity, state.map_center });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn init_world() void {
    ng.register_component("Node", state.Node);
    ng.register_component("Link", state.Link);
    ng.register_component("Construction", state.Construction);

    const n1 = ng.new();
    const n2 = ng.new();
    const n3 = ng.new();
    const l1 = ng.new();
    const n4 = ng.new();
    const n5 = ng.new();
    const l2 = ng.new();

    n1.set(state.Node{ .pos = .{ 10, 10 } });
    n2.set(state.Node{ .pos = .{ 20, 25 } });
    n3.set(state.Node{ .pos = .{ 30, 20 } });
    l1.set(state.Link{ .start = n1, .mid = n2, .end = n3, .width = 72 });
    n4.set(state.Node{ .pos = .{ 40, 10 } });
    n5.set(state.Node{ .pos = .{ 50, 15 } });
    l2.set(state.Link{ .start = n2, .mid = n4, .end = n5, .width = 72 });
    l2.set(state.Construction{ .step = 10, .steps = 30 });

    ng.dump_ecs();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
