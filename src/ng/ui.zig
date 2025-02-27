///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");
const ng = @import("ng");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const log = ng.Logger(.ng_ui);

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const debug_text_shader = @import("debug_text_shader.zig");
const DebugTextVertex = debug_text_shader.Vertex;
const DebugTextUniforms = ng.make_uniform_slots(debug_text_shader.Uniforms);

const debug_font = @import("debug_font.zig").debug_font;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

var gpa: std.heap.GeneralPurposeAllocator(.{}) = undefined;
var allocator: std.mem.Allocator = undefined;

var all_objects: std.ArrayListUnmanaged (Object) = .empty;
var used_objects: std.ArrayListUnmanaged (bool) = .empty;
const Handle = enum (u32) { _ };

var init_required: bool = false;

var first_window: ?Handle = null;
var last_window: ?Handle= null;

var clicked_window: ?Handle = null;

var shader: ng.video.Shader = undefined;
var buffer: ng.video.Buffer = undefined;
var image: ng.video.Image = undefined;
var sampler: ng.video.Sampler = undefined;
var binding: ng.video.Binding = undefined;
var pipeline: ng.video.Pipeline = undefined;

var vertices: [16384]DebugTextVertex = undefined;
var next_vertex: usize = 0;

var font_data: [256 * 12 * 20]u8 = undefined;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn init() !void {
    log.info("init", .{});

    gpa = std.heap.GeneralPurposeAllocator(.{}){};
    allocator = gpa.allocator();

    init_required = true;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn deinit() void {
    log.info("deinit", .{});

    shader.delete();
    image.delete();
    sampler.delete();

    all_objects.deinit (allocator);
    used_objects.deinit (allocator);

    std.debug.assert(gpa.deinit() == .ok);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn init_render() !void {
    if (!init_required) {
        return;
    }

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

    init_required = false;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn render(render_pass: ng.RenderPass) void {
    init_render() catch |err|
        {
            log.err("Failed to init {}", .{err});
            return;
        };

    const display_size = render_pass.get_size();

    dump_windows ("Render");

    // log.info ("Render {d}", .{display_size});

    var object = last_window;
    while (object) |handle| {
        var obj = get (handle) catch return;
        // log.info ("  {} {} {}", .{handle, obj.active, obj.shown});
        if (obj.shown)
        {
            switch (obj.data) {
                .window => |window| {
                    window.draw ();
                },
                else => {},
            }
        }

        obj.shown = obj.active;
        obj.active = false;

        object = obj.pred;
    }

    if (next_vertex > 0) {
        buffer.update(ng.as_bytes(vertices[0..next_vertex]));

        const mvp: ng.Mat4 = ng.ortho(display_size[0], display_size[1]);

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

pub const BeginWindowOptions = struct {
    unique: usize = 0,
    title: []const u8,
    x: ?f32 = null,
    y: ?f32 = null,
    width: ?f32 = null,
    height: ?f32 = null,
    background_color: ng.Color = .@"dark grey",
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn begin_window(options: BeginWindowOptions) void {
    const ident = Ident{ .addr = @returnAddress(), .unique = options.unique };

    if (find_window(ident)) |handle|
    {
        log.info ("Found {}", .{handle});
        var window = get (handle) catch return;
        if (window.shown == false) {
            move_to_top(handle);
        }
        window.active = true;
        window.shown = true;
    } else {
        const handle = new () catch |err| {
            log.err("begin_window {}", .{err});
            return;
        };

        const object = get (handle) catch return;

        object.* = .{
            .ident = ident,
            .data = .{
                .window = .{
                    .title = options.title,
                    .x = options.x orelse 100,
                    .y = options.y orelse 100,
                    .width = options.width orelse 320,
                    .height = options.width orelse 200,
                    .background_color = options.background_color,
                },
            },
        };

        move_to_top (handle);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn end_window() void {}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const BeginBoxOptions = struct {
    unique: usize = 0,
    direction: BoxDirection = .vertical,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const BoxDirection = enum {
    horizontal,
    vertical,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn begin_box(options: BeginBoxOptions) void {
    log.note("Box {}:{}", .{ @returnAddress(), options.unique });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn end_box() void {}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const FormatOptions = struct {
    unique: usize = 0,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn format_text(options: FormatOptions, comptime fmt: []const u8, args: anytype) void {
    log.note("Format Text {}:{}", .{ @returnAddress(), options.unique });
    _ = fmt;
    _ = args;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const Ident = struct {
    addr: usize,
    unique: usize,
};

const Object = struct {
    pred: ?Handle = null,
    succ: ?Handle = null,
    ident: Ident,
    active: bool = true, // active this frame
    shown: bool = true, // currently shown
    data: union(UI_Type) {
        window: Window,
        box: Box,
    },

    pub fn process_event (self: *Object, handle: Handle, event: ng.Event) bool
    {
        switch (self.data)
        {
            .window => |*win| return win.process_event (handle, event),
            else => {}
        }
        return false;
    }
};

const UI_Type = enum {
    window,
    box,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const Window = struct {
    title: []const u8,
    x: f32,
    y: f32,
    width: f32,
    height: f32,
    background_color: ng.Color,

    pub fn draw (self: Window) void
    {
        add_vertex(.{
            .pos = .{ self.x, self.y },
            .uv = .{ 0, 0 },
            .col = self.background_color,
        });
        add_vertex(.{
            .pos = .{ self.x + self.width, self.y },
            .uv = .{ 0, 0 },
            .col = self.background_color,
        });
        add_vertex(.{
            .pos = .{ self.x, self.y + self.height},
            .uv = .{ 0, 0 },
            .col = self.background_color,
        });
        add_vertex(.{
            .pos = .{ self.x, self.y + self.height},
            .uv = .{ 0, 0 },
            .col = self.background_color,
        });
        add_vertex(.{
            .pos = .{ self.x + self.width, self.y },
            .uv = .{ 0, 0 },
            .col = self.background_color,
        });
        add_vertex(.{
            .pos = .{ self.x + self.width, self.y + self.height},
            .uv = .{ 0, 0 },
            .col = self.background_color,
        });
    }

    pub fn process_event (self: *Window, handle: Handle, event: ng.Event) bool
    {
        switch (event)
        {
            .mouse_move => |ev| return self.process_mouse_move (handle, ev),
            .mouse_down => |ev| return self.process_mouse_down (handle, ev),
            .mouse_up => |ev| return self.process_mouse_up (handle, ev),
            else => {}
        }
        return false;
    }

    pub fn process_mouse_move (self: Window, handle: Handle, event: ng.MoveEvent) bool
    {
        _ = handle;

        if (event.x >= self.x and event.x < self.x + self.width)
        {
            if (event.y >= self.y and event.y < self.y + self.height)
            {
                return true;
            }
        }
        return false;
    }

    pub fn process_mouse_down (self: Window, handle: Handle, event: ng.MouseEvent) bool
    {
        if (event.x >= self.x and event.x < self.x + self.width)
        {
            if (event.y >= self.y and event.y < self.y + self.height)
            {
                clicked_window = handle;
                return true;
            }
        }
        return false;
    }

    pub fn process_mouse_up (self: Window, handle: Handle, _: ng.MouseEvent) bool
    {
        _ = self;
        if (clicked_window == handle)
        {
            clicked_window = null;
        }
        return false;
    }
};

const Box = struct {
    direction: BoxDirection,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn find_window(ident: Ident) ?Handle {
    var object = first_window;
    while (object) |handle| {
        const obj = get (handle) catch return null;
        if (obj.ident.addr == ident.addr and obj.ident.unique == ident.unique) {
            return handle;
        }
        object = obj.succ;
    }
    return null;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn move_to_top(handle: Handle) void {
    if (first_window == handle) {
        return;
    }

    var window = get (handle) catch return;

    if (window.pred) |pred_handle| {
        var pred = get (pred_handle) catch {
            log.err ("get pred_handle {}", .{pred_handle});
            return;
        };
        pred.succ = window.succ;
    }

    if (window.succ) |succ_handle| {
        var succ = get (succ_handle) catch {
            log.err ("get succ_handle {}", .{succ_handle});
            return;
        };
        succ.pred = window.pred;
    }

    if (last_window == handle)
    {
        last_window = window.pred;
    }

    window.pred = null;
    window.succ = null;

    if (first_window) |first_handle| {
        first_window = handle;
        window.succ = first_handle;
        var first = get (first_handle) catch {
            log.err ("get first_handle {}", .{first_handle});
            return;
        };
        first.pred = handle;
    } else {
        first_window = handle;
        last_window = handle;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn dump_windows (label: []const u8) void {
    ng.debug_print ("Dump Windows {s} : {?} : {?}\n", .{label, first_window, last_window});
    var window = first_window;
    while (window) |handle|
    {
        const obj = get(handle) catch return;
        ng.debug_print ("  {} {?} {?} {} {}\n", .{handle, obj.pred, obj.succ, obj.active, obj.shown});
        window = obj.succ;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn filter_event (ev: ng.Event) ?ng.Event
{
    var window = first_window;
    while (window) |handle|
    {
        var win = get (handle) catch return ev;

        if (win.shown)
        {
            if (win.process_event (handle, ev))
            {
                return null;
            }
        }

        window = win.succ;
    }

    return ev;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn new () !Handle
{
    const index = all_objects.items.len;

    try all_objects.append (allocator, undefined);
    try used_objects.append (allocator, true);

    return @enumFromInt (index);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn get (handle: Handle) !*Object
{
    const index = @intFromEnum (handle);
    if (index < used_objects.items.len and used_objects.items[index])
    {
        return &all_objects.items[index];
    }

    log.err ("Get {} invalid", .{handle});
    return error.InvalidHandle;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
