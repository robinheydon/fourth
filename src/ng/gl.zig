///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");
const ng = @import("ng");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const gl_shader_source = @import("gl_shader.zig");
const GL_Uniforms = ng.make_uniform_slots(gl_shader_source.Uniforms);
const Index = u32;

var gl_shader: ng.Shader = undefined;
var gl_buffer: ng.Buffer = undefined;
var gl_index: ng.Buffer = undefined;
var gl_binding: ng.Binding = undefined;
var gl_pipeline: ng.Pipeline = undefined;

var gl_vertexes: std.ArrayList(gl_shader_source.Vertex) = undefined;
var gl_indexes: std.ArrayList(Index) = undefined;
var current_color: ng.Color = undefined;
var current_mvp: ng.Mat4 = undefined;
var detail: f32 = undefined;
var quality: f32 = 80;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn init(allocator: std.mem.Allocator) !void {
    gl_shader = try ng.create_shader(gl_shader_source);

    gl_vertexes = std.ArrayList(gl_shader_source.Vertex).init(allocator);
    gl_indexes = std.ArrayList(Index).init(allocator);

    gl_buffer = try ng.create_buffer(.{
        .label = "gl vertex buffer",
        .kind = .vertex,
        .size = 1024 * 1024 * @sizeOf(gl_shader_source.Vertex),
        .update = .stream,
    });

    gl_index = try ng.create_buffer(.{
        .label = "gl index buffer",
        .kind = .index,
        .size = 1024 * 1024 * @sizeOf(Index),
        .update = .stream,
    });

    gl_binding = try ng.create_binding(.{
        .label = "gl binding",
        .vertex_buffers = &.{gl_buffer},
        .index_buffers = &.{gl_index},
    });

    gl_pipeline = try ng.create_pipeline(.{
        .label = "gl pipeline",
        .shader = gl_shader,
        .primitive = .triangle_list,
        .index_type = .u32,
    });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn deinit() void {
    gl_vertexes.deinit();
    gl_indexes.deinit();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn start_render(mvp: ng.Mat4) void {
    current_mvp = mvp;

    detail = @sqrt(@abs(mvp[0] * mvp[5] - mvp[1] * mvp[4]));
    ng.debug_print("{d} => {d}\n", .{ mvp, detail });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn draw_line(start: ng.Vec2, end: ng.Vec2, width: f32, color: ng.Color) !void {
    const delta = end - start;
    const dist = @sqrt(delta[0] * delta[0] + delta[1] * delta[1]);
    if (dist > 0) {
        const tangent = ng.Vec2{ delta[1], delta[0] } * @as(ng.Vec2, @splat(width / 2 / dist));

        set_color(color);
        const p0 = try add_vertex(start - tangent);
        const p1 = try add_vertex(start + tangent);
        const p2 = try add_vertex(end - tangent);
        const p3 = try add_vertex(end + tangent);

        try add_triangle(p0, p1, p2);
        try add_triangle(p2, p1, p3);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn fill_circle(pos: ng.Vec2, radius: f32, color: ng.Color) !void {
    const num: f32 = @min(256, @max(9, @sqrt(detail * radius) * quality));
    const a: f32 = std.math.tau / @floor(num);
    const r: ng.Vec2 = @splat(radius);

    ng.debug_print("fill_circle {d} {d} {x} ({d} {d})\n", .{
        pos,
        radius,
        color,
        detail,
        num,
    });

    set_color(color);

    var angle: f32 = 0;

    const first = ng.Vec2{ @cos(angle), @sin(angle) } * r;
    angle += a;

    const p0 = try add_vertex(pos);
    var p1 = try add_vertex(pos + first);
    const first_p1 = p1;

    const segments: usize = @as(usize, @intFromFloat(num)) - 1;

    for (0..segments) |_| {
        const cur = ng.Vec2{ @cos(angle), @sin(angle) } * r;
        const p2 = try add_vertex(pos + cur);
        try add_triangle(p0, p1, p2);
        p1 = p2;
        angle += a;
    }

    try add_triangle(p0, p1, first_p1);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn draw_circle(pos: ng.Vec2, radius: f32, width: f32, color: ng.Color) !void {
    const num: f32 = @min(256, @max(9, @sqrt(detail * radius) * quality));
    const a: f32 = std.math.tau / @floor(num);
    const r: ng.Vec2 = @splat(radius);
    const w: ng.Vec2 = @splat(width);

    ng.debug_print("fill_circle {d} {d} {x} ({d} {d})\n", .{
        pos,
        radius,
        color,
        detail,
        num,
    });

    set_color(color);

    var angle: f32 = 0;

    const inner = ng.Vec2{ @cos(angle), @sin(angle) } * (r - w);
    const outer = ng.Vec2{ @cos(angle), @sin(angle) } * r;
    var last_inner = try add_vertex(pos + inner);
    var last_outer = try add_vertex(pos + outer);
    const first_inner = last_inner;
    const first_outer = last_outer;
    angle += a;

    const segments: usize = @as(usize, @intFromFloat(num)) - 1;

    for (0..segments) |_| {
        const cur_inner = ng.Vec2{ @cos(angle), @sin(angle) } * (r - w);
        const cur_outer = ng.Vec2{ @cos(angle), @sin(angle) } * r;
        const p0 = try add_vertex(pos + cur_inner);
        const p1 = try add_vertex(pos + cur_outer);
        try add_triangle(last_inner, last_outer, p0);
        try add_triangle(last_outer, p0, p1);
        last_inner = p0;
        last_outer = p1;
        angle += a;
    }

    try add_triangle(last_inner, last_outer, first_inner);
    try add_triangle(last_outer, first_inner, first_outer);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn set_color(col: ng.Color) void {
    current_color = col;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn add_vertex(pos: ng.Vec2) !Index {
    const index: Index = @intCast(gl_vertexes.items.len);
    try gl_vertexes.append(.{ .pos = pos, .col = current_color });
    return index;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn add_triangle(p0: Index, p1: Index, p2: Index) !void {
    try gl_indexes.append(p0);
    try gl_indexes.append(p1);
    try gl_indexes.append(p2);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn render(render_pass: ng.RenderPass) void {
    gl_buffer.update(ng.as_bytes(gl_vertexes.items));
    gl_index.update(ng.as_bytes(gl_indexes.items));

    render_pass.apply_pipeline(gl_pipeline);
    render_pass.apply_bindings(gl_binding);
    render_pass.apply_uniform_mat4(GL_Uniforms.mvp, current_mvp);
    render_pass.draw(gl_indexes.items.len);

    gl_vertexes.clearRetainingCapacity();
    gl_indexes.clearRetainingCapacity();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
