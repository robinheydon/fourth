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

    init_roads();

    state.window = try ng.create_window(.{
        .name = "Fourth",
        .width = 1280,
        .height = 720,
        .resizable = true,
    });
    defer state.window.close();

    state.window.set_swap_interval(.lowpower);

    const triangle_shader = @import("triangle_shader.zig");
    const TriangleVertex = triangle_shader.Vertex;
    const TriangleUniforms = ng.make_uniform_slots(triangle_shader.Uniforms);

    const shader = try ng.create_shader(triangle_shader);
    defer shader.delete();

    // green - x-axis
    // red - y-axis
    const triangle_data = [_]TriangleVertex{
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

    const buffer = try ng.create_buffer(.{
        .label = "Triangle Vertex Data",
        .data = ng.as_bytes(&triangle_data),
    });
    defer buffer.delete();

    const pipeline = try ng.create_pipeline(.{
        .label = "Triangle Pipeline",
        .shader = shader,
        .primitive = .triangle_list,
    });
    defer pipeline.delete();

    const binding = try ng.create_binding(.{
        .label = "Triangle Bindings",
        .vertex_buffers = &.{buffer},
    });
    defer binding.delete();

    while (state.running) {
        state.frame_counter +%= 1;

        const dt = ng.start_frame();
        defer ng.end_frame();

        ng.debug_reset(state.window);

        if (state.average_frame_rate > 0) {
            ng.debug_print("{} Hz", .{state.average_frame_rate});
        }

        update_fps(dt);

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

        if (ng.is_key_down(state.key_move_left) or ng.is_key_down(state.key_move_left2)) {
            state.map_move_velocity[0] -= dt * 100 / state.map_zoom;
        } else if (ng.is_key_down(state.key_move_right) or ng.is_key_down(state.key_move_right2)) {
            state.map_move_velocity[0] += dt * 100 / state.map_zoom;
        } else {
            state.map_move_velocity[0] -= state.map_move_velocity[0] * 10 * dt;
            if (@abs(state.map_move_velocity[0]) < 0.1) {
                state.map_move_velocity[0] = 0;
            }
        }
        if (ng.is_key_down(state.key_move_down) or ng.is_key_down(state.key_move_down2)) {
            state.map_move_velocity[1] -= dt * 100 / state.map_zoom;
        } else if (ng.is_key_down(state.key_move_up) or ng.is_key_down(state.key_move_up2)) {
            state.map_move_velocity[1] += dt * 100 / state.map_zoom;
        } else {
            state.map_move_velocity[1] -= state.map_move_velocity[1] * 10 * dt;
            if (@abs(state.map_move_velocity[1]) < 0.1) {
                state.map_move_velocity[1] = 0;
            }
        }

        state.map_center += state.map_move_velocity * ng.Vec2{ dt, dt };

        const window_size = state.window.get_size();
        const projection = ng.ortho(window_size.width, window_size.height);

        state.camera = .identity();
        state.camera.zoom = state.map_zoom;
        state.camera.rotate = state.map_rotate;
        state.camera.origin = state.map_center;
        state.camera.target = .{ window_size.width / 2, window_size.height / 2 };
        const view = state.camera.get_matrix();
        const mvp = ng.mat4_mul(view, projection);

        debug_map_state();

        const command_buffer = try state.window.acquire_command_buffer();
        const swapchain_texture = try command_buffer.acquire_swapchain_texture();
        const render_pass = try command_buffer.begin_render_pass(.{
            .texture = swapchain_texture,
            .clear_color = .black,
            .load = .clear,
            .store = .store,
        });

        render_pass.apply_pipeline(pipeline);
        render_pass.apply_bindings(binding);
        render_pass.apply_uniform_mat4(TriangleUniforms.mvp, mvp);
        render_pass.draw(triangle_data.len);

        ng.debug_text_draw();

        render_pass.end();
        try command_buffer.submit();
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_key_down(event: ng.KeyEvent) void {
    if (event.key == state.key_quit) {
        state.running = false;
    } else if (event.key == state.key_toggle_fullscreen) {
        state.window.toggle_fullscreen();
    } else {
        // log.debug("{} ({})", .{ event.key, event.scan_code });
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_mouse_move(event: ng.MoveEvent) void {
    if (state.map_state == .clicked) {
        const dx = @abs(state.map_start_click_x - event.x);
        const dy = @abs(state.map_start_click_y - event.y);

        if (dx > 8 or dy > 8) {
            state.map_state = .dragging;
        }
    }

    if (state.map_state == .dragging) {
        const start_position = state.camera.to_world(state.map_last_mouse);
        const end_position = state.camera.to_world(.{ event.x, event.y });
        const delta_pos = start_position - end_position;

        state.map_center += delta_pos;
    }

    state.map_last_mouse = .{ event.x, event.y };
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
    log.info("down {} {d} {d}", .{ event.button, event.x, event.y });
    if (event.button == .left) {
        state.map_state = .clicked;
        state.map_start_click_x = event.x;
        state.map_start_click_y = event.y;
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
    const multiplier = std.math.pow(f32, 1.2, event.dy);
    state.map_zoom = std.math.clamp(state.map_zoom * multiplier, 0.1, 100);
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
    const position = state.camera.to_world(state.map_last_mouse);

    ng.debug_print("{d:8.5} -> {d:8.5}\n", .{ state.map_last_mouse, position });
    ng.debug_print("{d:8.5} {d:8.5}\n", .{ state.map_move_velocity, state.map_center });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn init_roads() void {
    ng.register_component("Node", state.Node);
    ng.register_component("Link", state.Link);
    ng.register_component("Construction", state.Construction);
    ng.register_component("Health", state.Health);

    const n1 = ng.new();
    const n2 = ng.new();
    const n3 = ng.new();
    const l1 = ng.new();
    const n4 = ng.new();
    const n5 = ng.new();
    const l2 = ng.new();

    const h1 = ng.new();
    const h2 = ng.new();
    const h3 = ng.new();

    h1.set(state.Health{ .hp = 50, .max = 100 });
    h2.set(state.Health{ .hp = 100, .max = 110 });
    h2.set(state.Construction{ .step = 20, .steps = 40 });
    h3.set(state.Construction{ .step = 0, .steps = 100 });

    log.info("h1.health = {?}", .{h1.get(state.Health)});
    log.info("h2.health = {?}", .{h2.get(state.Health)});
    log.info("h3.health = {?}", .{h3.get(state.Health)});

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
