///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const ng = @import("ng.zig");

const debug_text_shader = @import("debug_text_shader.zig");
const DebugTextVertex = debug_text_shader.Vertex;
const DebugTextUniforms = ng.make_uniform_slots(debug_text_shader.Uniforms);

const debug_font = @import("debug_font.zig").debug_font;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

var shader: ng.video.Shader = undefined;
var buffer: ng.video.Buffer = undefined;
var image: ng.video.Image = undefined;
var sampler: ng.video.Sampler = undefined;
var binding: ng.video.Binding = undefined;
var pipeline: ng.video.Pipeline = undefined;
var render_pass: ng.video.RenderPass = undefined;

var vertices: [16384]DebugTextVertex = undefined;
var next_vertex: usize = 0;

var font_data: [256 * 12 * 20]u8 = undefined;

var cursor_x: usize = 0;
var cursor_y: usize = 0;
var frame: [4096]u8 = undefined;
var frame_width: usize = 0;
var frame_height: usize = 0;
var frame_dx: f32 = 0;
var frame_dy: f32 = 0;
var frame_offset_x: f32 = 0.5;
var frame_offset_y: f32 = 0.5;
var debug_scale: f32 = 4;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn init() !void {
    std.debug.print("debug_text.init\n", .{});

    buffer = try ng.create_buffer(.{
        .label = "debug text vertex buffer",
        .kind = .vertex,
        .size = @sizeOf(DebugTextVertex) * vertices.len,
        .update = .stream,
    });

    shader = try ng.create_shader(debug_text_shader);

    for (0..256) |ch| {
            const tx = ch & 15;
            const ty = ch / 16;
            const pitch = 16 * 12;
            const to = ty * pitch * 20 + tx * 12;
            for (0..20) |y| {
                const bits = debug_font[ch * 20 + y];
                var bit: u12 = 0x800;
                for (0..12) |x| {
                    var byte: u8 = 0;
                    if (bits & bit != 0) {
                        byte = 255;
                    }
                    font_data[to + y * pitch + x] = byte;
                    bit >>= 1;
                }
            }
        
    }

    image = try ng.create_image(.{
        .label = "debug text font image",
        .width = 16 * 12,
        .height = 16 * 20,
        .format = .r8,
        .data = &font_data,
    });

    sampler = try ng.create_sampler(.{
        .label = "debug text sampler",
        .min_filter = .nearest,
        .mag_filter = .nearest,
        .wrap_u = .clamp_to_edge,
        .wrap_v = .clamp_to_edge,
    });

    binding = try ng.create_binding(.{
        .label = "debug text binding",
        .vertex_buffers = &.{buffer},
        .image = image,
        .sampler = sampler,
    });

    pipeline = try ng.create_pipeline(.{
        .label = "debug text pipeline",
        .shader = shader,
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

pub fn deinit() void {
    std.debug.print("debug_text.deinit\n", .{});

    shader.delete();
    image.delete();
    sampler.delete();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn reset(width: usize, height: usize) void {
    cursor_x = 0;
    cursor_y = 0;
    @memset(&frame, 0);
    const fwidth: f32 = @floatFromInt(width);
    const fheight: f32 = @floatFromInt(height);

    if (fwidth > 12 * debug_scale and fheight > 20 * debug_scale) {
        frame_width = @intFromFloat(fwidth / 12 / debug_scale - 1);
        frame_height = @intFromFloat(fheight / 20 / debug_scale - 1);
    } else {
        frame_width = 1;
        frame_height = 1;
    }

    frame_dx = 2.0 / fwidth * debug_scale;
    frame_dy = 2.0 / fheight * debug_scale;
}

pub fn puts(str: []const u8) void {
    for (str) |ch| {
        if (ch == '\n') {
            cursor_x = 0;
            cursor_y += 1;
        } else if (ch < 32) {
        } else {
            if (cursor_y < frame_height) {
                if (frame_width * cursor_y + cursor_x < frame.len) {
                    frame[frame_width * cursor_y + cursor_x] = ch;
                    cursor_x += 1;
                    if (cursor_x >= frame_width) {
                        cursor_x = 0;
                        cursor_y += 1;
                    }
                }
            }
        }
    }
}

pub fn print(comptime fmt: []const u8, args: anytype) void {
    const debug_writer: void = {};
    const writer = std.io.AnyWriter{ .context = &debug_writer, .writeFn = debug_text_writer };
    std.fmt.format(writer, fmt, args) catch {};
}

fn debug_text_writer(self: *const anyopaque, bytes: []const u8) error{}!usize {
    _ = self;
    puts(bytes);
    return bytes.len;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn draw() void {
    // std.debug.print ("Draw Debug Text\n", .{});

    for (0..frame_height) |y| {
        for (0..frame_width) |x| {
            const index = frame_width * y + x;
            if (index >= frame.len) {
                break;
            }
            const ch = frame[index];
            if (ch < 33) {
                continue;
            } else {
                const fx: f32 = @as(f32, @floatFromInt(x)) + frame_offset_x;
                const fy: f32 = @as(f32, @floatFromInt(y)) + frame_offset_x;
                const px1: f32 = fx * 12 * frame_dx - 1;
                const py1: f32 = 1 - fy * 20 * frame_dy;
                const px2: f32 = (fx + 1) * 12 * frame_dx - 1;
                const py2: f32 = 1 - (fy + 1) * 20 * frame_dy;

                const cx: f32 = @floatFromInt(ch & 15);
                const cy: f32 = @floatFromInt(ch / 16);
                const cx1: f32 = (cx * 12) / (16 * 12);
                const cy1: f32 = (cy * 20) / (16 * 20);
                const cx2: f32 = ((cx + 1) * 12) / (16 * 12);
                const cy2: f32 = ((cy + 1) * 20) / (16 * 20);

                add_vertex(.{
                    .pos = .{ px1, py1 },
                    .uv = .{ cx1, cy1 },
                    .col = .white,
                });
                add_vertex(.{
                    .pos = .{ px2, py1 },
                    .uv = .{ cx2, cy1 },
                    .col = .white,
                });
                add_vertex(.{
                    .pos = .{ px1, py2 },
                    .uv = .{ cx1, cy2 },
                    .col = .white,
                });
                add_vertex(.{
                    .pos = .{ px1, py2 },
                    .uv = .{ cx1, cy2 },
                    .col = .white,
                });
                add_vertex(.{
                    .pos = .{ px2, py1 },
                    .uv = .{ cx2, cy1 },
                    .col = .white,
                });
                add_vertex(.{
                    .pos = .{ px2, py2 },
                    .uv = .{ cx2, cy2 },
                    .col = .red,
                });
            }
        }
    }

    if (next_vertex > 0) {
        buffer.update(ng.as_bytes(vertices[0..next_vertex]));

        const mvp: ng.Mat4 = ng.mat4_identity;

        render_pass.apply_pipeline(pipeline);
        render_pass.apply_bindings(binding);
        render_pass.apply_uniform_mat4(DebugTextUniforms.mvp, mvp);
        render_pass.apply_uniform_u32(DebugTextUniforms.smp, 0);
        render_pass.draw(next_vertex);
        next_vertex = 0;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn add_vertex(vertex: DebugTextVertex) void {
    if (next_vertex < vertices.len) {
        vertices[next_vertex] = vertex;
        next_vertex += 1;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
