///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

const ng = @import("ng");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

var running = true;

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
        .width = 1920,
        .height = 1080,
        .resizable = true,
    });
    defer window.close();

    window.set_swap_interval(.lowpower);

    const triangle_shader = @import("triangle_shader.zig");
    const TriangleVertex = triangle_shader.Vertex;

    const shader = try ng.create_shader(triangle_shader);
    defer shader.delete();

    const TriangleUniforms = ng.make_uniform_slots(triangle_shader.Uniforms);

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

    const bindings = try ng.create_bindings(.{
        .label = "Triangle Bindings",
        .vertex_buffers = &.{buffer},
    });

    var camera: ng.Camera2D = .identity();

    while (running) {
        const dt = ng.start_frame();
        defer ng.end_frame();

        report_fps(dt);

        while (ng.poll_event()) |event| {
            switch (event) {
                .quit => {
                    running = false;
                },
                .key_down => |key_event| {
                    if (key_event.key == 9) {
                        running = false;
                    }
                },
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
        render_pass.apply_bindings(bindings);
        render_pass.apply_uniform(TriangleUniforms.mvp, &mvp);
        render_pass.draw(3);

        render_pass.end();
        try command_buffer.submit();
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

var all_delta_times: [65536]f32 = undefined;
var next_dt_index: usize = 0;

fn report_fps(dt: f32) void {
    all_delta_times[next_dt_index] = dt;
    next_dt_index = next_dt_index + 1;
    var total_ft: f32 = 0;
    for (all_delta_times[0..next_dt_index]) |ft| {
        total_ft += ft;
    }
    if (total_ft >= 1.0 or next_dt_index == all_delta_times.len) {
        average_frame_rate = @intCast(next_dt_index);
        std.debug.print("{} Hz\n", .{average_frame_rate});
        next_dt_index = 0;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
