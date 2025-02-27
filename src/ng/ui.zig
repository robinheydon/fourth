///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");
const ng = @import("ng");

const Vec2 = ng.Vec2;

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

var all_objects: std.ArrayListUnmanaged(Object) = .empty;
var used_objects: std.ArrayListUnmanaged(bool) = .empty;
const Handle = enum(u32) { _ };

var init_required: bool = false;

var first_window: ?Handle = null;
var last_window: ?Handle = null;

var hover: ?Handle = null;
var captured_mouse: ?Handle = null;
var window_resizing: ResizingMode = .none;
var captured_offset: Vec2 = .{ 0, 0 };
var app_captured_mouse: bool = false;

var shader: ng.video.Shader = undefined;
var buffer: ng.video.Buffer = undefined;
var image: ng.video.Image = undefined;
var sampler: ng.video.Sampler = undefined;
var binding: ng.video.Binding = undefined;
var pipeline: ng.video.Pipeline = undefined;

var vertices: [16384]DebugTextVertex = undefined;
var next_vertex: usize = 0;

var font_data: [256 * 12 * 20]u8 = undefined;

const ResizingMode = enum {
    none,
    resize,
    top,
    left,
    bottom,
    right,
};

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

    all_objects.deinit(allocator);
    used_objects.deinit(allocator);

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

    var object = first_window;
    while (object) |handle| {
        var obj = get(handle) catch return;
        switch (obj.data) {
            .window => |*window| {
                window.fit_to_display(display_size);
            },
            else => {},
        }
        object = obj.succ;
    }

    dump_state("Render");

    object = last_window;
    while (object) |handle| {
        var obj = get(handle) catch return;
        if (obj.shown) {
            switch (obj.data) {
                .window => |window| {
                    window.draw();
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
    min_width: ?f32 = null,
    min_height: ?f32 = null,
    max_width: ?f32 = null,
    max_height: ?f32 = null,
    background_color: ng.Color = .@"dark grey",
    resize_handle_color: ng.Color = .white,
    resize_handle_size: f32 = 20,
    resize_border_size: f32 = 10,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn begin_window(options: BeginWindowOptions) void {
    const ident = Ident{ .addr = @returnAddress(), .unique = options.unique };

    if (find_window(ident)) |handle| {
        var window = get(handle) catch return;
        if (window.shown == false) {
            move_to_top(handle);
        }
        window.active = true;
        window.shown = true;
    } else {
        const handle = new() catch |err| {
            log.err("begin_window {}", .{err});
            return;
        };

        const object = get(handle) catch return;

        object.* = .{
            .ident = ident,
            .data = .{
                .window = .{
                    .title = options.title,
                    .pos = .{
                        options.x orelse 100,
                        options.y orelse 100,
                    },
                    .size = .{
                        options.width orelse 320,
                        options.width orelse 200,
                    },
                    .min_size = .{
                        options.min_width orelse 120,
                        options.min_height orelse 80,
                    },
                    .max_size = .{
                        options.max_width orelse 800,
                        options.max_height orelse 800,
                    },
                    .background_color = options.background_color,
                    .resize_handle_color = options.resize_handle_color,
                    .resize_handle_size = options.resize_handle_size,
                    .resize_border_size = options.resize_border_size,
                },
            },
        };

        move_to_top(handle);
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

pub noinline fn format_text(
    options: FormatOptions,
    comptime fmt: []const u8,
    args: anytype,
) void {
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

    pub fn process_event(self: *Object, handle: Handle, event: ng.Event) bool {
        switch (self.data) {
            .window => |*win| return win.process_event(handle, event),
            else => {},
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
    pos: Vec2,
    size: Vec2,
    min_size: Vec2,
    max_size: Vec2,
    background_color: ng.Color,
    resize_handle_color: ng.Color,
    resize_handle_size: f32,
    resize_border_size: f32,

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn draw(self: Window) void {
        add_vertex(.{
            .pos = self.pos,
            .uv = .{ 0, 0 },
            .col = self.background_color,
        });
        add_vertex(.{
            .pos = self.pos + Vec2{ self.size[0], 0 },
            .uv = .{ 0, 0 },
            .col = self.background_color,
        });
        add_vertex(.{
            .pos = self.pos + Vec2{ 0, self.size[1] },
            .uv = .{ 0, 0 },
            .col = self.background_color,
        });
        add_vertex(.{
            .pos = self.pos + Vec2{ 0, self.size[1] },
            .uv = .{ 0, 0 },
            .col = self.background_color,
        });
        add_vertex(.{
            .pos = self.pos + Vec2{ self.size[0], 0 },
            .uv = .{ 0, 0 },
            .col = self.background_color,
        });
        add_vertex(.{
            .pos = self.pos + self.size,
            .uv = .{ 0, 0 },
            .col = self.background_color,
        });

        add_vertex(.{
            .pos = self.pos + self.size - Vec2{ 0, self.resize_handle_size },
            .uv = .{ 0, 0 },
            .col = self.resize_handle_color,
        });

        add_vertex(.{
            .pos = self.pos + self.size,
            .uv = .{ 0, 0 },
            .col = self.resize_handle_color,
        });

        add_vertex(.{
            .pos = self.pos + self.size - Vec2{ self.resize_handle_size, 0 },
            .uv = .{ 0, 0 },
            .col = self.resize_handle_color,
        });
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn fit_to_display(self: *Window, size: Vec2) void {
        switch (window_resizing) {
            .none => {
                if (self.pos[0] + self.size[0] > size[0]) self.pos[0] = size[0] - self.size[0];
                if (self.pos[1] + self.size[1] > size[1]) self.pos[1] = size[1] - self.size[1];
                if (self.pos[0] < 0) self.pos[0] = 0;
                if (self.pos[1] < 0) self.pos[1] = 0;
            },
            .resize => {
                if (self.pos[0] + self.size[0] > size[0]) {
                    self.size[0] = size[0] - self.pos[0];
                }
                if (self.pos[1] + self.size[1] > size[1]) {
                    self.size[1] = size[1] - self.pos[1];
                }
            },
            .top => {
                if (self.pos[1] < 0) {
                    self.size[1] += self.pos[1];
                    self.pos[1] = 0;
                }
            },
            .left => {
                if (self.pos[0] < 0) {
                    self.size[0] += self.pos[0];
                    self.pos[0] = 0;
                }
            },
            .bottom => {
                if (self.pos[1] + self.size[1] > size[1]) {
                    self.size[1] = size[1] - self.pos[1];
                }
            },
            .right => {
                if (self.pos[0] + self.size[0] > size[0]) {
                    self.size[0] = size[0] - self.pos[0];
                }
            },
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn process_event(self: *Window, handle: Handle, event: ng.Event) bool {
        switch (event) {
            .mouse_move => |ev| return self.process_mouse_move(handle, ev),
            .mouse_down => |ev| return self.process_mouse_down(handle, ev),
            .mouse_up => |ev| return self.process_mouse_up(handle, ev),
            .key_down, .key_up => return false,
            else => {},
        }
        return false;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn process_mouse_move(self: *Window, handle: Handle, event: ng.MoveEvent) bool {
        if (captured_mouse == handle) {
            switch (window_resizing) {
                .none => {
                    self.pos = event.pos - captured_offset;
                    ng.use_cursor(.move);
                },
                .resize => {
                    self.size = std.math.clamp(
                        event.pos - self.pos,
                        self.min_size,
                        self.max_size,
                    );
                    ng.use_cursor(.resize);
                },
                .top => {
                    const pos = event.pos[1] - captured_offset[1];
                    const size = std.math.clamp(
                        self.pos[1] + self.size[1] - pos,
                        self.min_size[1],
                        self.max_size[1],
                    );
                    self.pos[1] = self.pos[1] + self.size[1] - size;
                    self.size[1] = size;
                    ng.use_cursor(.resize_ns);
                },
                .left => {
                    const pos = event.pos[0] - captured_offset[0];
                    const size = std.math.clamp(
                        self.pos[0] + self.size[0] - pos,
                        self.min_size[0],
                        self.max_size[0],
                    );
                    self.pos[0] = self.pos[0] + self.size[0] - size;
                    self.size[0] = size;
                    ng.use_cursor(.resize_ew);
                },
                .bottom => {
                    self.size[1] = std.math.clamp(
                        event.pos[1] - self.pos[1],
                        self.min_size[1],
                        self.max_size[1],
                    );
                    ng.use_cursor(.resize_ns);
                },
                .right => {
                    self.size[0] = std.math.clamp(
                        event.pos[0] - self.pos[0],
                        self.min_size[0],
                        self.max_size[0],
                    );
                    ng.use_cursor(.resize_ew);
                },
            }
        } else {
            if (event.pos[0] >= self.pos[0] and
                event.pos[0] < self.pos[0] + self.size[0] and
                event.pos[1] >= self.pos[1] and
                event.pos[1] < self.pos[1] + self.size[1])
            {
                const bottom_right = self.pos + self.size - event.pos;
                const top_left = event.pos - self.pos;
                if (bottom_right[0] + bottom_right[1] < self.resize_handle_size) {
                    ng.use_cursor(.resize);
                } else if (bottom_right[0] < self.resize_border_size) {
                    ng.use_cursor(.resize_ew);
                } else if (bottom_right[1] < self.resize_border_size) {
                    ng.use_cursor(.resize_ns);
                } else if (top_left[0] < self.resize_border_size) {
                    ng.use_cursor(.resize_ew);
                } else if (top_left[1] < self.resize_border_size) {
                    ng.use_cursor(.resize_ns);
                } else {
                    ng.use_cursor(.default);
                }
                hover = handle;
                return true;
            }
            hover = null;
        }
        return false;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn process_mouse_down(self: Window, handle: Handle, event: ng.MouseEvent) bool {
        if (event.button == .left) {
            if (event.pos[0] >= self.pos[0] and
                event.pos[0] < self.pos[0] + self.size[0] and
                event.pos[1] >= self.pos[1] and
                event.pos[1] < self.pos[1] + self.size[1])
            {
                const bottom_right = self.pos + self.size - event.pos;
                const top_left = event.pos - self.pos;
                if (bottom_right[0] + bottom_right[1] < self.resize_handle_size) {
                    window_resizing = .resize;
                } else if (bottom_right[0] < self.resize_border_size) {
                    window_resizing = .right;
                } else if (top_left[0] < self.resize_border_size) {
                    window_resizing = .left;
                } else if (bottom_right[1] < self.resize_border_size) {
                    window_resizing = .bottom;
                } else if (top_left[1] < self.resize_border_size) {
                    window_resizing = .top;
                } else {
                    window_resizing = .none;
                }
                captured_mouse = handle;
                captured_offset = event.pos - self.pos;
                move_to_top(handle);
                return true;
            }
        }
        return false;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn process_mouse_up(self: Window, handle: Handle, event: ng.MouseEvent) bool {
        _ = self;
        if (event.button == .left) {
            if (captured_mouse == handle) {
                captured_mouse = null;
                return true;
            }
        }
        return false;
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const Box = struct {
    direction: BoxDirection,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn find_window(ident: Ident) ?Handle {
    var object = first_window;
    while (object) |handle| {
        const obj = get(handle) catch return null;
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

    var window = get(handle) catch return;

    if (window.pred) |pred_handle| {
        var pred = get(pred_handle) catch {
            log.err("get pred_handle {}", .{pred_handle});
            return;
        };
        pred.succ = window.succ;
    }

    if (window.succ) |succ_handle| {
        var succ = get(succ_handle) catch {
            log.err("get succ_handle {}", .{succ_handle});
            return;
        };
        succ.pred = window.pred;
    }

    if (last_window == handle) {
        last_window = window.pred;
    }

    window.pred = null;
    window.succ = null;

    if (first_window) |first_handle| {
        first_window = handle;
        window.succ = first_handle;
        var first = get(first_handle) catch {
            log.err("get first_handle {}", .{first_handle});
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

fn dump_state(label: []const u8) void {
    ng.debug_print("{s} : {?} : {?} : {?} : {} : {?}\n", .{
        label,
        first_window,
        last_window,
        captured_mouse,
        app_captured_mouse,
        hover,
    });
    var window = first_window;
    while (window) |handle| {
        const obj = get(handle) catch return;
        ng.debug_print("  {} {?} {?} {} {}", .{
            handle,
            obj.pred,
            obj.succ,
            obj.active,
            obj.shown,
        });
        switch (obj.data) {
            .window => |win| {
                ng.debug_print(" {d} {d}", .{
                    win.pos,
                    win.size,
                });
            },
            else => {},
        }
        ng.debug_print("\n", .{});
        window = obj.succ;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn new() !Handle {
    const index = all_objects.items.len;

    try all_objects.append(allocator, undefined);
    try used_objects.append(allocator, true);

    return @enumFromInt(index);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn get(handle: Handle) !*Object {
    const index = @intFromEnum(handle);
    if (index < used_objects.items.len and used_objects.items[index]) {
        return &all_objects.items[index];
    }

    log.err("Get {} invalid", .{handle});
    return error.InvalidHandle;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn is_hover() bool {
    if (hover) |handle| {
        _ = get(handle) catch {
            hover = null;
            return false;
        };
        return true;
    }
    return false;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn filter_event(event: ng.Event) ?ng.Event {
    if (captured_mouse) |handle| {
        var obj = get(handle) catch {
            captured_mouse = null;
            return event;
        };
        if (obj.process_event(handle, event)) {
            return null;
        }
        return event;
    }

    if (app_captured_mouse) {
        switch (event) {
            .mouse_up => |ev| {
                if (ev.button == .left) {
                    app_captured_mouse = false;
                }
            },
            else => {},
        }
        return event;
    }

    var window = first_window;
    while (window) |handle| {
        var win = get(handle) catch return event;

        if (win.shown) {
            if (win.process_event(handle, event)) {
                return null;
            }
        }

        window = win.succ;
    }

    switch (event) {
        .mouse_down => |ev| {
            if (ev.button == .left) {
                app_captured_mouse = true;
            }
        },
        .text => {
            return null;
        },
        else => {},
    }

    ng.use_cursor(.default);

    return event;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
