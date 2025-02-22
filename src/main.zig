///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

const ng = @import("ng");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

var running = true;

var frame_counter: usize = 0;
var average_frame_rate: u32 = 0;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn main() !void {
    try ng.init(.{
        .video = true,
        .audio = true,
    });
    defer ng.deinit();

    const window = try ng.create_window(.{
        .name = "Fourth",
        .width = 1280,
        .height = 720,
        .resizable = true,
    });
    defer window.close();

    window.set_swap_interval(.lowpower);

    const triangle_shader = @import("triangle_shader.zig");
    const TriangleVertex = triangle_shader.Vertex;
    const TriangleUniforms = ng.make_uniform_slots(triangle_shader.Uniforms);

    const shader = try ng.create_shader(triangle_shader);
    defer shader.delete();

    const triangle_data = [_]TriangleVertex{
        .{ .pos = .{ 0, 50 }, .col = .red },
        .{ .pos = .{ 43, -25 }, .col = .green },
        .{ .pos = .{ -43, -25 }, .col = .blue },
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

    var camera: ng.Camera2D = .identity();

    while (running) {
        frame_counter +%= 1;

        const dt = ng.start_frame();
        defer ng.end_frame();

        ng.debug_reset(window);

        if (average_frame_rate > 0) {
            ng.debug_print("{} Hz\n", .{average_frame_rate});
        }

        update_fps(dt);

        while (ng.poll_event()) |event| {
            switch (event) {
                .quit => {
                    running = false;
                },
                .key_down => |key_event| {
                    if (key_event.key == .escape) {
                        running = false;
                    } else if (key_event.key == .f11) {
                        window.toggle_fullscreen();
                    } else {
                        std.debug.print("{} ({})\n", .{ key_event.key, key_event.scan_code });
                    }
                },
                .key_up => {},
                .resize => {},
                .enter => {},
                .leave => {},
                .focus => {},
                .unfocus => {},
                else => {
                    std.debug.print("{}\n", .{event});
                },
            }
        }

        const window_size = window.get_size();
        const projection = ng.ortho(window_size.width, window_size.height);

        const now: f32 = @floatCast(ng.elapsed());
        camera.zoom = @sin(now / 5) + 2;
        camera.rotate = @cos(now / 3);
        camera.origin = .{ window_size.width / 2, window_size.height / 2 };
        camera.target = .{ @sin(now * 3) * 500, @cos(now * 3) * 500 };
        const view = camera.get_matrix();
        const mvp = ng.mat4_mul(view, projection);

        const command_buffer = try window.acquire_command_buffer();
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
        render_pass.draw(3);

        for (32..255) |ch| {
            ng.debug_print("{c}", .{@as(u8, @intCast(ch))});
        }

        ng.debug_text_draw();

        render_pass.end();
        try command_buffer.submit();
    }
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
        average_frame_rate = @intCast(next_dt_index);
        next_dt_index = 0;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
