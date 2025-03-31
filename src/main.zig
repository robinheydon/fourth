///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

const ng = @import("ng");

const gl = ng.gl;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const state = @import("state.zig");
const com = @import("com.zig");

var allocator: std.mem.Allocator = undefined;

pub const log = ng.Logger(.main);

var debug_state: bool = false;
var debug_timer: f32 = 0;
var slow_frame_rate: bool = false;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    try ng.init(.{
        .video = true,
        .audio = false,
    });

    defer ng.deinit();

    log.set_min_level(.note);
    log.info("starting", .{});
    defer log.info("ending", .{});

    state.window = try ng.create_window(.{
        .name = "Fourth",
        .width = 1920,
        .height = 1080,
        .resizable = true,
    });
    defer state.window.close();

    // state.window.toggle_fullscreen();

    state.window.set_swap_interval(.lowpower);

    init_world();

    try init_draw_world();

    try gl.init(allocator);
    defer gl.deinit();

    while (state.running) {
        state.frame_counter +%= 1;

        state.dt = ng.start_frame();
        ng.debug_clear(state.window);

        update_fps(state.dt);

        const command_buffer = try state.window.acquire_command_buffer();
        const swapchain_texture = try command_buffer.acquire_swapchain_texture();
        const render_pass = try command_buffer.begin_render_pass(.{
            .texture = swapchain_texture,
            .clear_color = .@"dark grey",
            .load = .clear,
            .store = .store,
        });

        start_draw_world(render_pass) catch {};

        ng.progress(state.dt);

        process_events();

        {
            const world_position = state.camera.to_world(state.map_last_mouse);
            if (find_nearest_link(world_position)) |entity| {
                _ = entity;
            }
        }

        draw_ui();

        end_draw_world(render_pass) catch {};

        ng.ui_render(render_pass);

        ng.debug_text_draw(render_pass);

        render_pass.end();
        try command_buffer.submit();

        if (slow_frame_rate) {
            ng.sleep(0.5);
        }

        ng.end_frame();
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
        .x = 400,
        .y = 300,
        .width = 320,
        .height = 200,
        .background_color = .black,
    });
    defer ng.ui_end_window();

    ng.ui_begin_vbox(.{ .padding = .{ .top = 4, .left = 4, .right = 4, .bottom = 4 } });
    // ng.ui_begin_hbox(.{ .padding = .{ .top = 4, .left = 4, .right = 4, .bottom = 4 }});
    ng.ui_add_text(.{}, "{} fps", .{state.average_frame_rate});
    ng.ui_add_text(.{}, "{} frames", .{state.frame_counter});
    ng.ui_add_text(.{}, "{} clicks", .{state.button_clicks});
    // ng.ui.end_hbox ();

    if (ng.ui_add_button(.{
        .text = "Button",
        .width = 100,
        .height = 40,
        .padding = .{ .top = 8, .left = 8, .right = 8, .bottom = 8 },
    })) |count| {
        state.button_clicks += count;
    }

    if (state.button_clicks > 0 and state.button_clicks < 30) {
        ng.ui_add_text(.{}, "abcdefghijklmnopqrstuvwxyz", .{});
    }
    if (state.button_clicks > 10) ng.ui_add_text(.{}, "ABCDEFGHIJKLMNOPQRSTUVWXYZ", .{});
    if (state.button_clicks > 20) ng.ui_add_text(.{}, "0123456789", .{});

    if (debug_state) {
        ng.ui_add_text(.{}, "State On {d:0.3}", .{debug_timer});
    } else {
        ng.ui_add_text(.{}, "State Off {d:0.3}", .{debug_timer});
    }

    debug_timer -= state.dt;
    if (debug_timer < 0) {
        debug_timer += 1.0;
        debug_state = !debug_state;
    }

    ng.ui_end_vbox();
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
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn start_draw_world(render_pass: ng.RenderPass) !void {
    const window_size = state.window.get_size();
    const projection = ng.ortho(window_size);

    state.camera = .identity();
    state.camera.zoom = state.map_zoom;
    state.camera.rotate = state.map_rotate;
    state.camera.origin = state.map_center;
    state.camera.target = window_size / ng.Vec2{ 2, 2 };
    const view = state.camera.get_matrix();
    const mvp = ng.mat4_mul(view, projection);

    debug_map_state(window_size);

    render_pass.apply_pipeline(state.grid_pipeline);
    render_pass.apply_uniform_mat4(state.GridUniforms.mvp, mvp);
    render_pass.draw(6);

    gl.start_render(mvp);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn end_draw_world(render_pass: ng.RenderPass) !void {
    gl.render(render_pass);
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
    } else if (event.key == .tab) {
        slow_frame_rate = !slow_frame_rate;
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

    if (state.map_selected_state == .clicked) {
        const delta = @abs(state.map_selected_click - event.pos);

        if (delta[0] > 8 or delta[1] > 8) {
            state.map_selected_state = .dragging;
        }
    }

    if (state.map_selected_state == .dragging) {
        const world_position = state.camera.to_world(event.pos);
        if (state.map_selected) |entity| {
            if (entity.getPtr(com.Node)) |node| {
                node.pos = world_position;
            }
            var iter = state.links_query.iterator();
            while (iter.next()) |link_entity| {
                if (link_entity.get(com.Link)) |link| {
                    if (link.start == entity) {
                        // link_entity.set(com.Construction{ .steps = 10 });
                        if (link.start.has(com.Curve)) {
                            link.start.set(com.CurveUpdateRequired{});
                        }
                        if (link.end.has(com.Curve)) {
                            link.end.set(com.CurveUpdateRequired{});
                        }
                    }
                    if (link.end == entity) {
                        // link_entity.set(com.Construction{ .steps = 10 });
                        if (link.start.has(com.Curve)) {
                            link.start.set(com.CurveUpdateRequired{});
                        }
                        if (link.end.has(com.Curve)) {
                            link.end.set(com.CurveUpdateRequired{});
                        }
                    }
                }
            }
        }
    }
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
    switch (event.button) {
        .left => {
            const map_pos = state.camera.to_world(event.pos);
            if (find_nearest_node(map_pos)) |entity| {
                state.map_selected = entity;
            } else if (find_nearest_link(map_pos)) |entity| {
                state.map_selected = entity;
            } else {
                state.map_selected = null;
                state.map_state = .clicked;
                state.map_start_click = event.pos;
            }
            state.map_selected_click = event.pos;
            state.map_selected_state = .clicked;
        },
        .right => {
            state.map_state = .clicked;
            state.map_start_click = event.pos;
        },
        else => {},
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_mouse_up(event: ng.MouseEvent) void {
    switch (event.button) {
        .left => {
            state.map_selected_state = .none;
            state.map_state = .none;
        },
        .right => {
            state.map_state = .none;
        },
        else => {},
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_wheel_event(event: ng.WheelEvent) void {
    const min_zoom = 0.1; // a few 10km squares
    const max_zoom = 1000; // a few 1m squares

    const multiplier = std.math.pow(f32, 1.1, event.dy);
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

fn debug_map_state(window_size: ng.Vec2) void {
    const top_left = state.camera.to_world(ng.Vec2{ 0, 0 });
    const bottom_right = state.camera.to_world(window_size);
    ng.debug_print("{d:8.2} .. {d:8.2}\n", .{ top_left, bottom_right });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn init_world() void {
    ng.register_component("VehicleKind", com.VehicleKind);
    ng.register_component("Node", com.Node);
    ng.register_component("Link", com.Link);
    ng.register_component("LaneKind", com.LaneKind);
    ng.register_component("LaneStyle", com.LaneStyle);
    ng.register_component("Curve", com.Curve);
    ng.register_component("CurveUpdateRequired", com.CurveUpdateRequired);
    ng.register_component("Construction", com.Construction);
    ng.register_component("Position", com.Position);
    ng.register_component("Velocity", com.Velocity);

    ng.register_system(
        .{
            .name = "update_curves",
            .phase = .pre_update,
        },
        update_curves_system,
        .{
            com.CurveUpdateRequired,
            *com.Curve,
        },
    );

    ng.register_system(
        .{
            .name = "movement_system",
            .phase = .update,
        },
        movement_system,
        .{
            *com.Position,
            com.Velocity,
        },
    );

    ng.register_system(
        .{
            .name = "construction_system",
            .phase = .update,
        },
        construction_system,
        .{
            com.Construction,
        },
    );

    ng.register_system(
        .{
            .name = "draw_links",
            .phase = .render0,
        },
        draw_links_system,
        .{
            com.Link,
        },
    );

    ng.register_system(
        .{
            .name = "draw_nodes",
            .phase = .render1,
        },
        draw_nodes_system,
        .{
            com.Node,
        },
    );

    ng.register_system(
        .{
            .name = "autosave",
            .phase = .last_phase,
            .interval = 1,
        },
        autosave_system,
        .{},
    );

    state.nodes_query = ng.register_query(
        .{
            .name = "nodes",
        },
        .{
            com.Node,
        },
    );

    state.links_query = ng.register_query(
        .{
            .name = "links",
        },
        .{
            com.Link,
        },
    );

    const n1 = ng.new();
    const n2 = ng.new();
    const n3 = ng.new();
    const n4 = ng.new();
    const n5 = ng.new();
    const l1 = ng.new();
    const l2 = ng.new();
    const l3 = ng.new();
    const l4 = ng.new();
    const s1 = ng.new();

    n1.set(com.Node{ .pos = .{ -20, 10 } });

    n2.set(com.Node{ .pos = .{ 50, 20 } });
    n2.set(com.Curve{ .radius = 20, .from = l1, .to = l2 });
    n2.set(com.CurveUpdateRequired{});

    n3.set(com.Node{ .pos = .{ 100, 60 } });
    n3.set(com.Curve{ .radius = 20, .from = l2, .to = l3 });
    n3.set(com.CurveUpdateRequired{});

    n4.set(com.Node{ .pos = .{ 140, 50 } });
    n4.set(com.Curve{ .radius = 15, .from = l3, .to = l4 });
    n4.set(com.CurveUpdateRequired{});

    n5.set(com.Node{ .pos = .{ 200, 50 } });

    l1.set(com.Link{ .start = n1, .end = n2, .width = 120, .style = s1 });
    l2.set(com.Link{ .start = n2, .end = n3, .width = 120, .style = s1 });
    l3.set(com.Link{ .start = n3, .end = n4, .width = 120, .style = s1 });
    l4.set(com.Link{ .start = n4, .end = n5, .width = 120, .style = s1 });

    add_lane(s1, .sidewalk, 2.0);
    add_lane(s1, .traffic_lane_up, 3.2);
    add_lane(s1, .traffic_lane_down, 3.2);
    add_lane(s1, .sidewalk, 2.0);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn construction_system(iter: *const ng.SystemIterator) void {
    for (iter.entities) |entity| {
        if (entity.get(com.Construction)) |con| {
            const new_step = con.step + iter.delta_time;
            if (new_step >= con.steps) {
                entity.remove(com.Construction);
            } else {
                entity.set(com.Construction{ .step = new_step, .steps = con.steps });
            }
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn movement_system(iter: *const ng.SystemIterator) void {
    const dt: ng.Vec2 = @splat(iter.delta_time);

    for (iter.entities) |entity| {
        if (entity.getPtr(com.Position)) |position| {
            if (entity.get(com.Velocity)) |velocity| {
                position.pos += velocity.vel * dt;
            }
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn update_curves_system(iter: *const ng.SystemIterator) void {
    for (iter.entities) |entity| {
        const node = entity.get(com.Node) orelse continue;

        const curve = entity.getPtr(com.Curve) orelse continue;

        const radius: ng.Vec2 = @splat(curve.radius);

        const from_link = curve.from.get(com.Link) orelse return;
        const from = if (from_link.start == entity) from_link.end else from_link.start;

        const to_link = curve.to.get(com.Link) orelse return;
        const to = if (to_link.start == entity) to_link.end else to_link.start;
        const from_node = from.get(com.Node) orelse return;
        const to_node = to.get(com.Node) orelse return;

        const dfrom = ng.normalize(from_node.pos - node.pos);
        const dto = ng.normalize(node.pos - to_node.pos);

        const a = ng.atan2(dfrom);
        const b = ng.atan2(dto);
        const c = @mod(a - b, std.math.tau);

        const side: ng.Vec2 = if (c > std.math.pi) .{ 1, 1 } else .{ -1, -1 };

        const from_a = node.pos + ng.Vec2{ dfrom[1], -dfrom[0] } * radius * side;
        const from_b = from_node.pos + ng.Vec2{ dfrom[1], -dfrom[0] } * radius * side;

        const to_a = node.pos + ng.Vec2{ dto[1], -dto[0] } * radius * side;
        const to_b = to_node.pos + ng.Vec2{ dto[1], -dto[0] } * radius * side;

        curve.center = ng.line_intersection(from_a, from_b, to_a, to_b);

        const from_tangent = ng.Vec2{ -dfrom[1], dfrom[0] };
        const to_tangent = ng.Vec2{ -dto[1], dto[0] };

        const end_link = curve.center + from_tangent * radius * side;

        curve.start_angle = ng.atan2(from_tangent * side);
        curve.end_angle = ng.atan2(to_tangent * side);
        curve.clockwise = side[0] < 0;
        curve.width = from_link.width;
        curve.style = from_link.style;
        curve.offset = ng.distance(end_link - node.pos);

        entity.remove(com.CurveUpdateRequired);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn draw_nodes_system(iter: *const ng.SystemIterator) void {
    for (iter.entities) |entity| {
        if (entity.get(com.Node)) |node| {
            if (entity.get(com.Curve)) |curve| {
                draw_curve(curve) catch {};
            }
            if (entity == state.map_selected) {
                gl.draw_circle(node.pos, 5, 0.75, .white) catch {};
            }
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn draw_curve(curve: com.Curve) !void {
    const width = @as(f32, @floatFromInt(curve.width)) * 0.1;

    try gl.draw_arc(
        curve.center,
        curve.radius + width / 2,
        curve.start_angle,
        curve.end_angle,
        curve.clockwise,
        width,
        .black,
    );
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn draw_links_system(iter: *const ng.SystemIterator) void {
    var color: ng.Color = undefined;
    var construction: f32 = undefined;

    for (iter.entities) |entity| {
        if (entity == state.map_selected) {
            color = .white;
        } else {
            continue;
        }
        if (entity.get(com.Link)) |link| {
            const start = link.start;
            const end = link.end;
            const width = @as(f32, @floatFromInt(link.width)) * 0.1;
            // const w2: ng.Vec2 = @splat(width / 2);

            const start_node = start.get(com.Node);
            const end_node = end.get(com.Node);

            if (start_node) |n0| {
                if (end_node) |n2| {
                    gl.draw_line(n0.pos, n2.pos, width + 1, color) catch {};
                }
            }
        }
    }

    for (iter.entities) |entity| {
        if (entity.get(com.Construction)) |constr| {
            construction = @max(0, (constr.step / constr.steps - 0.2) * (1.0 / 0.8));
        } else {
            construction = 1;
        }
        if (entity.get(com.Link)) |link| {
            const start = link.start;
            const end = link.end;
            const width = @as(f32, @floatFromInt(link.width)) * 0.1;
            // const w2: ng.Vec2 = @splat(width / 2);

            const start_node = start.get(com.Node);
            const end_node = end.get(com.Node);
            const start_curve = start.get(com.Curve);
            const end_curve = end.get(com.Curve);

            const start_offset = if (start_curve) |curve| curve.offset else 0;
            const end_offset = if (end_curve) |curve| curve.offset else 0;

            const so = ng.Vec2{ start_offset, start_offset };
            const eo = ng.Vec2{ end_offset, end_offset };

            if (start_node) |n0| {
                if (end_node) |n2| {
                    if (construction < 1) {
                        const dn = ng.normalize(n2.pos - n0.pos);
                        gl.draw_line(
                            n0.pos + dn * so,
                            n2.pos - dn * eo,
                            width,
                            .grey,
                        ) catch {};
                        gl.draw_line(
                            n0.pos + dn * so,
                            n2.pos - dn * eo,
                            width * construction,
                            .black,
                        ) catch {};
                    } else {
                        const dn = ng.normalize(n2.pos - n0.pos);
                        gl.draw_line(
                            n0.pos + dn * so,
                            n2.pos - dn * eo,
                            width,
                            .black,
                        ) catch {};
                    }
                }
            }
        }
    }

    for (iter.entities) |entity| {
        if (entity == state.map_selected) {
            color = .white;
        } else {
            continue;
        }
        if (entity.get(com.Link)) |link| {
            const start = link.start;
            const end = link.end;

            const start_node = start.get(com.Node);
            const end_node = end.get(com.Node);

            if (start_node) |n0| {
                if (end_node) |n2| {
                    gl.draw_circle(n0.pos, 5, 0.5, .purple) catch {};
                    gl.draw_circle(n2.pos, 5, 0.5, .purple) catch {};
                }
            }
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn autosave_system(_: *const ng.SystemIterator) void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const save_allocator = gpa.allocator();
    defer std.debug.assert(gpa.deinit() == .ok);

    const memory = ng.save(save_allocator) catch |err| {
        log.err("Failed to save {}", .{err});
        return;
    };

    defer save_allocator.free(memory);

    const cwd = std.fs.cwd();
    var file = cwd.createFile("autosave.dat", .{}) catch |err| {
        log.err("Failed to createFile 'autosave.dat' {}", .{err});
        return;
    };
    defer file.close();

    _ = file.write(memory) catch |err| {
        log.err("Failed to write 'autosave.dat' {}", .{err});
        return;
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn find_nearest_node(pos: ng.Vec2) ?ng.Entity {
    var iter = state.nodes_query.iterator();
    var best_entity: ?ng.Entity = null;
    var best_dist: f32 = 0;
    while (iter.next()) |entity| {
        if (entity.get(com.Node)) |node| {
            const dist = ng.distance(node.pos - pos);
            if (dist <= 5 and (best_entity == null or dist < best_dist)) {
                best_dist = dist;
                best_entity = entity;
            }
        }
    }
    return best_entity;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn find_nearest_link(pos: ng.Vec2) ?ng.Entity {
    var iter = state.links_query.iterator();

    var best_entity: ?ng.Entity = null;
    var best_dist: f32 = undefined;

    while (iter.next()) |entity| {
        if (entity.get(com.Link)) |link| {
            const start = link.start;
            const end = link.end;
            const width = @as(f32, @floatFromInt(link.width)) * 0.1;

            const w2 = ng.Vec2{ width / 2, width / 2 };

            const start_node = start.get(com.Node);
            const end_node = end.get(com.Node);

            if (start_node) |n0| {
                if (end_node) |n1| {
                    const tl = @min(n0.pos - w2, n1.pos - w2);
                    const br = @max(n0.pos + w2, n1.pos + w2);

                    if (@reduce(.And, pos >= tl) and @reduce(.And, pos <= br)) {
                        const dist = ng.distance_to_line(pos, n0.pos, n1.pos);
                        if (dist <= width / 2 and
                            (best_entity == null or dist < best_dist))
                        {
                            best_dist = dist;
                            best_entity = entity;
                        }
                    }
                }
            }
        }
    }
    return best_entity;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn add_lane(entity: ng.Entity, kind: com.LaneKind, width: f32) void {
    if (entity.getPtr(com.LaneStyle)) |style| {
        if (style.num_lanes < com.max_lanes) {
            style.kind[style.num_lanes] = kind;
            style.width[style.num_lanes] = @intFromFloat(width / 0.1);
            style.num_lanes += 1;
        }
    } else {
        var style: com.LaneStyle = undefined;
        style.num_lanes = 1;
        style.kind[0] = kind;
        style.width[0] = @intFromFloat(width / 0.1);

        entity.set(style);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
