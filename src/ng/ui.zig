///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");
const ng = @import("ng");

const Vec2 = ng.Vec2;
const Color = ng.Color;

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

var init_required: bool = false;

var build_stack: [16]Handle = undefined;
var build_stack_index: usize = 0;

var first_window: ?Handle = null;
var last_window: ?Handle = null;

var hover: ?Handle = null;
var captured_mouse: ?Handle = null;
var window_resizing: ResizingMode = .none;
var captured_offset: Vec2 = .{ 0, 0 };
var app_captured_mouse: bool = false;

var ui_shader: ng.video.Shader = undefined;
var ui_buffer: ng.video.Buffer = undefined;
var ui_image: ng.video.Image = undefined;
var ui_sampler: ng.video.Sampler = undefined;
var ui_binding: ng.video.Binding = undefined;
var ui_pipeline: ng.video.Pipeline = undefined;

var vertices: [16384]DebugTextVertex = undefined;
var next_vertex: usize = 0;

var commands: [4096]RenderCommand = undefined;
var next_command: usize = 0;

const RenderCommand = union(RenderCommandKind) {
    none: void,
    draw_triangles: struct { start: usize, end: usize },
    scissor: struct { pos: ng.Vec2, size: ng.Vec2 },
};

const RenderCommandKind = enum {
    none,
    draw_triangles,
    scissor,
};

const ResizingMode = enum {
    none,
    resize,
    top,
    left,
    bottom,
    right,
};

var font_data: [256 * 12 * 20]u8 = undefined;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn init() !void {
    gpa = std.heap.GeneralPurposeAllocator(.{}){};
    allocator = gpa.allocator();

    init_required = true;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn deinit() void {
    log.info("deinit", .{});

    ui_shader.delete();
    ui_image.delete();
    ui_sampler.delete();

    for (all_objects.items) |obj| {
        switch (obj.data) {
            .text => |text| {
                if (text.allocated) {
                    allocator.free(text.memory);
                }
            },
            else => {},
        }
    }

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

    ui_buffer = try ng.create_buffer(.{
        .label = "UI vertex buffer",
        .kind = .vertex,
        .size = @sizeOf(DebugTextVertex) * vertices.len,
        .update = .stream,
    });

    ui_shader = try ng.create_shader(debug_text_shader);

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

    ui_image = try ng.create_image(.{
        .label = "UI font image",
        .width = 16 * 12,
        .height = 16 * 20,
        .format = .r8,
        .data = &font_data,
    });

    ui_sampler = try ng.create_sampler(.{
        .label = "UI sampler",
        .min_filter = .nearest,
        .mag_filter = .nearest,
        .wrap_u = .clamp_to_edge,
        .wrap_v = .clamp_to_edge,
    });

    ui_binding = try ng.create_binding(.{
        .label = "UI binding",
        .vertex_buffers = &.{ui_buffer},
        .image = ui_image,
        .sampler = ui_sampler,
    });

    ui_pipeline = try ng.create_pipeline(.{
        .label = "UI pipeline",
        .shader = ui_shader,
        .primitive = .triangle_list,
        .blend = .{
            .enabled = true,
            .src_factor_rgb = .src_alpha,
            .dst_factor_rgb = .one_minus_src_alpha,
            .src_factor_alpha = .one,
            .dst_factor_alpha = .zero,
        },
        .scissor_test = true,
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
        obj.fit_to_display(display_size);

        _ = obj.layout(handle, .{});

        object = obj.succ;
    }

    if (false) {
        dump_state("Render");
    }

    object = last_window;
    while (object) |handle| {
        var obj = get(handle) catch return;
        if (obj.shown) {
            switch (obj.data) {
                .window => |window| {
                    window.draw(obj);
                    draw_internal(obj.first_child);
                },
                else => {
                    log.warn("Cannot draw {s}", .{@tagName(obj.data)});
                },
            }
        }

        obj.shown = obj.active;
        obj.active = false;

        object = obj.pred;
    }

    if (next_vertex > 0) {
        ui_buffer.update(ng.as_bytes(vertices[0..next_vertex]));
    }

    const mvp: ng.Mat4 = ng.ortho(display_size);

    render_pass.apply_pipeline(ui_pipeline);
    render_pass.apply_bindings(ui_binding);
    render_pass.apply_uniform_mat4(DebugTextUniforms.mvp, mvp);
    render_pass.apply_uniform_u32(DebugTextUniforms.smp, 0);

    for (0..next_command) |i| {
        const command = commands[i];

        switch (command) {
            .none => {},
            .scissor => |cmd| {
                render_pass.apply_scissor(cmd.pos, cmd.size);
            },
            .draw_triangles => |cmd| {
                render_pass.draw_subset(cmd.start, cmd.end - cmd.start);
            },
        }
    }

    next_vertex = 0;
    next_command = 0;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn draw_internal(first_child: ?Handle) void {
    var child: ?Handle = first_child;
    while (child) |handle| {
        const obj = get(handle) catch return;
        if (obj.shown) {
            switch (obj.data) {
                .window => {},
                .vbox => {},
                .hbox => {},
                .text => |text| {
                    text.draw(obj);
                },
                .button => |button| {
                    button.draw(obj);
                },
            }
            draw_internal(obj.first_child);
        }
        child = obj.succ;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn add_vertex(vertex: DebugTextVertex) void {
    if (next_command == 0) {
        commands[next_command] = .{ .draw_triangles = .{
            .start = next_vertex,
            .end = next_vertex,
        } };
        next_command += 1;
    } else {
        if (commands[next_command - 1] != .draw_triangles) {
            if (next_command < commands.len) {
                commands[next_command] = .{ .draw_triangles = .{
                    .start = next_vertex,
                    .end = next_vertex,
                } };
                next_command += 1;
            }
        }
    }

    if (next_vertex < vertices.len) {
        vertices[next_vertex] = vertex;
        next_vertex += 1;
        commands[next_command - 1].draw_triangles.end = next_vertex;
    }
}

fn add_scissor(pos: ng.Vec2, size: ng.Vec2) void {
    if (next_command < commands.len) {
        commands[next_command] = .{ .scissor = .{ .pos = pos, .size = size } };
        next_command += 1;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const BeginWindowOptions = struct {
    unique: usize = 0,
    title_bar: bool = true,
    close_button: bool = true,
    title: []const u8,
    x: ?f32 = null,
    y: ?f32 = null,
    width: ?f32 = null,
    height: ?f32 = null,
    min_width: ?f32 = null,
    min_height: ?f32 = null,
    max_width: ?f32 = null,
    max_height: ?f32 = null,
    padding: Padding = .{ .left = 0, .top = 0, .right = 0, .bottom = 0 },
    background_color: ng.Color = .@"dark grey",
    title_bar_height: f32 = 20 + 8,
    title_bar_color: ng.Color = .@"very dark blue",
    title_text_color: ng.Color = .yellow,
    resize_handle_color: ng.Color = .white,
    resize_handle_size: f32 = 20,
    resize_border_size: f32 = 10,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Padding = struct {
    left: f32 = 0,
    top: f32 = 0,
    right: f32 = 0,
    bottom: f32 = 0,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn begin_window(options: BeginWindowOptions) void {
    const ident = Ident{ .addr = @returnAddress(), .unique = options.unique };

    while (!build_stack_empty()) {
        log.err("Build stack not empty", .{});
        pop_build_stack();
    }

    if (find_window(ident)) |handle| {
        var window = get(handle) catch return;
        if (window.shown == false) {
            move_to_top(handle);
        }
        window.active = true;
        window.shown = true;

        push_build_stack(handle);
    } else {
        const handle = new() catch |err| {
            log.err("begin_window {}", .{err});
            return;
        };

        const object = get(handle) catch return;

        object.* = .{
            .ident = ident,
            .pos = .{
                options.x orelse 100,
                options.y orelse 100,
            },
            .size = .{
                options.width orelse 320,
                options.height orelse 200,
            },
            .data = .{
                .window = .{
                    .title = options.title,
                    .min_size = .{
                        options.min_width orelse 120,
                        options.min_height orelse 80,
                    },
                    .max_size = .{
                        options.max_width orelse 800,
                        options.max_height orelse 800,
                    },
                    .background_color = options.background_color,
                    .title_bar_color = options.title_bar_color,
                    .title_bar_height = options.title_bar_height,
                    .title_text_color = options.title_text_color,
                    .resize_handle_color = options.resize_handle_color,
                    .resize_handle_size = options.resize_handle_size,
                    .resize_border_size = options.resize_border_size,
                },
            },
        };

        move_to_top(handle);

        push_build_stack(handle);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn end_window() void {
    pop_build_stack();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const BeginBoxOptions = struct {
    unique: usize = 0,
    padding: Padding = .{},
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn begin_vbox(options: BeginBoxOptions) void {
    const ident = Ident{ .addr = @returnAddress(), .unique = options.unique };

    const parent = top_build_stack();

    if (find_object(parent, ident)) |handle| {
        var object = get(handle) catch return;
        object.active = true;
        object.shown = true;
        push_build_stack(handle);
    } else {
        const handle = new() catch |err| {
            log.err("begin_box {}", .{err});
            return;
        };

        const object = get(handle) catch return;

        object.* = .{
            .ident = ident,
            .active = true,
            .shown = true,
            .padding = options.padding,
            .data = .{
                .vbox = .{},
            },
        };

        add_child_last(parent, handle);

        push_build_stack(handle);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn end_vbox() void {
    pop_build_stack();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn begin_hbox(options: BeginBoxOptions) void {
    const ident = Ident{ .addr = @returnAddress(), .unique = options.unique };

    const parent = top_build_stack();

    if (find_object(parent, ident)) |handle| {
        var object = get(handle) catch return;
        object.active = true;
        object.shown = true;
        push_build_stack(handle);
    } else {
        const handle = new() catch |err| {
            log.err("begin_box {}", .{err});
            return;
        };

        const object = get(handle) catch return;

        object.* = .{
            .ident = ident,
            .active = true,
            .shown = true,
            .padding = options.padding,
            .data = .{
                .hbox = .{},
            },
        };

        add_child_last(parent, handle);

        push_build_stack(handle);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn end_hbox() void {
    pop_build_stack();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const FormatOptions = struct {
    unique: usize = 0,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn add_text(
    options: FormatOptions,
    comptime fmt: []const u8,
    args: anytype,
) void {
    const ident = Ident{ .addr = @returnAddress(), .unique = options.unique };
    add_text_internal(options, fmt, args, ident);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn add_text_internal(
    options: FormatOptions,
    comptime fmt: []const u8,
    args: anytype,
    ident: Ident,
) void {
    _ = options;
    const parent = top_build_stack();

    if (find_object(parent, ident)) |handle| {
        var object = get(handle) catch return;
        object.active = true;
        object.shown = true;

        switch (object.data) {
            .text => |*text| {
                if (text.allocated) {
                    const count = std.fmt.count(fmt, args);

                    if (count <= text.memory.len) {
                        const slice = std.fmt.bufPrint(text.memory, fmt, args) catch return;

                        text.text = slice;
                    } else {
                        const block_count = (count + 7) & 0xFFFF_FFFF_FFFF_FFF8;
                        const buffer = allocator.alloc(u8, block_count) catch return;
                        const slice = std.fmt.bufPrint(buffer, fmt, args) catch return;

                        allocator.free(text.memory);
                        text.memory = buffer;
                        text.text = slice;
                    }
                }
            },
            else => {},
        }
    } else {
        const count = std.fmt.count(fmt, args);
        const block_count = (count + 7) & 0xFFFF_FFFF_FFFF_FFF8;
        const buffer = allocator.alloc(u8, block_count) catch return;
        const slice = std.fmt.bufPrint(buffer, fmt, args) catch return;

        const handle = new() catch |err| {
            log.err("text {}", .{err});
            return;
        };

        const object = get(handle) catch return;

        object.* = .{
            .ident = ident,
            .active = true,
            .shown = true,
            .data = .{
                .text = .{
                    .memory = buffer,
                    .text = slice,
                    .allocated = true,
                },
            },
        };

        add_child_last(parent, handle);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const AddButtonOptions = struct {
    unique: usize = 0,
    text: []const u8,
    width: f32 = 100,
    height: f32 = 40,
    padding: Padding = .{},
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn add_button(
    options: AddButtonOptions,
) bool {
    const ident = Ident{ .addr = @returnAddress(), .unique = options.unique };

    const clicked = begin_button_internal(.{
        .unique = options.unique,
        .width = options.width,
        .height = options.height,
    }, ident);

    begin_hbox(.{ .padding = .{
        .left = options.padding.left,
        .top = options.padding.top,
        .right = options.padding.right,
        .bottom = options.padding.bottom,
    } });
    add_text_internal(.{}, "{s}", .{options.text}, ident);
    end_hbox();

    end_button();

    return clicked;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const ButtonOptions = struct {
    unique: usize = 0,
    width: f32 = 100,
    height: f32 = 40,
};

pub noinline fn begin_button(
    options: ButtonOptions,
) bool {
    const ident = Ident{ .addr = @returnAddress(), .unique = options.unique };

    return begin_button_internal(options, ident);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn begin_button_internal(options: ButtonOptions, ident: Ident) bool {
    const parent = top_build_stack();

    if (find_object(parent, ident)) |handle| {
        var object = get(handle) catch return false;
        object.active = true;
        object.shown = true;

        push_build_stack(handle);

        return false;
    } else {
        const handle = new() catch |err| {
            log.err("button {}", .{err});
            return false;
        };

        const object = get(handle) catch return false;

        object.* = .{
            .ident = ident,
            .active = true,
            .shown = true,
            .data = .{
                .button = .{
                    .min_size = .{ options.width, options.height },
                },
            },
        };

        add_child_last(parent, handle);

        push_build_stack(handle);
    }

    return false;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn end_button() void {
    pop_build_stack();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn push_build_stack(handle: Handle) void {
    if (build_stack_index < build_stack.len) {
        build_stack[build_stack_index] = handle;
        build_stack_index += 1;
    } else {
        log.err("Build stack overflow {}", .{handle});
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn pop_build_stack() void {
    if (build_stack_index > 0) {
        build_stack_index -= 1;
    } else {
        log.err("Build stack underflow", .{});
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn build_stack_empty() bool {
    return build_stack_index == 0;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn top_build_stack() Handle {
    if (build_stack_index > 0) {
        return build_stack[build_stack_index - 1];
    } else {
        log.err("Build stack underflow", .{});
        return build_stack[0];
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const Ident = struct {
    addr: usize,
    unique: usize,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const Handle = enum(u32) {
    _,
    pub fn children(self: Handle) !Object.ObjectChildrenIterator {
        const obj = try get(self);
        return obj.children();
    }

    pub fn layout(self: Handle, constraint: LayoutConstraint) ng.Vec2 {
        const obj = get(self) catch return .{ 0, 0 };
        return obj.layout(self, constraint);
    }

    pub fn format(self: Handle, _: anytype, _: anytype, writer: anytype) !void {
        try writer.print("#{d}", .{@intFromEnum(self)});
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const UI_Type = enum {
    window,
    vbox,
    hbox,
    text,
    button,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const Object = struct {
    pred: ?Handle = null,
    succ: ?Handle = null,
    first_child: ?Handle = null,
    last_child: ?Handle = null,
    ident: Ident,
    active: bool = true, // active this frame
    shown: bool = true, // currently shown
    pos: Vec2 = .{ 0, 0 },
    size: Vec2 = .{ 0, 0 },
    padding: Padding = .{},
    data: union(UI_Type) {
        window: Window,
        vbox: VBox,
        hbox: HBox,
        text: Text,
        button: Button,
    },

    const ObjectChildrenIterator = struct {
        obj: *const Object,
        child: ?Handle = null,

        pub fn next(self: *ObjectChildrenIterator) ?Handle {
            if (self.child) |child| {
                const obj = get(child) catch return null;
                self.child = obj.succ;
                return child;
            }
            return null;
        }
    };

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn children(self: *Object) ObjectChildrenIterator {
        return .{
            .obj = self,
            .child = self.first_child,
        };
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn fit_to_display(self: *Object, size: Vec2) void {
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

    pub fn process_event(self: *Object, handle: Handle, event: ng.Event) bool {
        switch (self.data) {
            .window => |*win| return win.process_event(handle, event),
            .button => |*button| return button.process_event(handle, event),
            else => {},
        }
        return false;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn layout(self: *const Object, handle: Handle, constraint: LayoutConstraint) ng.Vec2 {
        if (self.shown) {
            return switch (self.data) {
                .window => |*win| win.layout(handle, constraint),
                .vbox => |*vbox| vbox.layout(handle, constraint),
                .hbox => |*hbox| hbox.layout(handle, constraint),
                .text => |*text| text.layout(handle, constraint),
                .button => |*button| button.layout(handle, constraint),
            };
        }
        return .{ 0, 0 };
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const LayoutConstraint = struct {
    min_size: ng.Vec2 = .{ 0, 0 },
    max_size: ng.Vec2 = .{ 0, 0 },

    pub fn format(self: LayoutConstraint, _: anytype, _: anytype, writer: anytype) !void {
        try writer.print("Constraint({d},{d}..{d},{d})", .{
            self.min_size[0],
            self.min_size[1],
            self.max_size[0],
            self.max_size[1],
        });
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const Window = struct {
    title: []const u8,
    min_size: Vec2,
    max_size: Vec2,
    background_color: ng.Color,
    title_bar_color: ng.Color,
    title_bar_height: f32,
    title_text_color: ng.Color,
    resize_handle_color: ng.Color,
    resize_handle_size: f32,
    resize_border_size: f32,

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn draw(self: Window, obj: *const Object) void {
        add_scissor(obj.pos, obj.size);
        draw_rectangle(obj.pos, obj.size, self.background_color);

        if (false) // let's not draw this anymore
        {
            add_vertex(.{
                .pos = obj.pos + obj.size - Vec2{ 0, self.resize_handle_size },
                .uv = .{ 0, 0 },
                .col = self.resize_handle_color,
            });

            add_vertex(.{
                .pos = obj.pos + obj.size,
                .uv = .{ 0, 0 },
                .col = self.resize_handle_color,
            });

            add_vertex(.{
                .pos = obj.pos + obj.size - Vec2{ self.resize_handle_size, 0 },
                .uv = .{ 0, 0 },
                .col = self.resize_handle_color,
            });
        }

        draw_rectangle(
            obj.pos,
            Vec2{ obj.size[0], self.title_bar_height },
            self.title_bar_color,
        );

        draw_text(
            obj.pos + Vec2{ 4, 4 },
            self.title,
            self.title_bar_height - 8,
            self.title_text_color,
        );
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
            var obj = get(handle) catch return false;
            switch (window_resizing) {
                .none => {
                    obj.pos = event.pos - captured_offset;
                    ng.use_cursor(.move);
                },
                .resize => {
                    obj.size = std.math.clamp(
                        event.pos - obj.pos,
                        self.min_size,
                        self.max_size,
                    );
                    ng.use_cursor(.resize);
                },
                .top => {
                    const new_pos = event.pos[1] - captured_offset[1];
                    const new_size = std.math.clamp(
                        obj.pos[1] + obj.size[1] - new_pos,
                        self.min_size[1],
                        self.max_size[1],
                    );
                    obj.pos[1] = obj.pos[1] + obj.size[1] - new_size;
                    obj.size[1] = new_size;
                    ng.use_cursor(.resize_ns);
                },
                .left => {
                    const new_pos = event.pos[0] - captured_offset[0];
                    const new_size = std.math.clamp(
                        obj.pos[0] + obj.size[0] - new_pos,
                        self.min_size[0],
                        self.max_size[0],
                    );
                    obj.pos[0] = obj.pos[0] + obj.size[0] - new_size;
                    obj.size[0] = new_size;
                    ng.use_cursor(.resize_ew);
                },
                .bottom => {
                    obj.size[1] = std.math.clamp(
                        event.pos[1] - obj.pos[1],
                        self.min_size[1],
                        self.max_size[1],
                    );
                    ng.use_cursor(.resize_ns);
                },
                .right => {
                    obj.size[0] = std.math.clamp(
                        event.pos[0] - obj.pos[0],
                        self.min_size[0],
                        self.max_size[0],
                    );
                    ng.use_cursor(.resize_ew);
                },
            }
        } else {
            const obj = get(handle) catch return false;
            if (event.pos[0] >= obj.pos[0] and
                event.pos[0] < obj.pos[0] + obj.size[0] and
                event.pos[1] >= obj.pos[1] and
                event.pos[1] < obj.pos[1] + obj.size[1])
            {
                const bottom_right = obj.pos + obj.size - event.pos;
                const top_left = event.pos - obj.pos;
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
        const obj = get(handle) catch return false;
        if (event.button == .left) {
            if (event.pos[0] >= obj.pos[0] and
                event.pos[0] < obj.pos[0] + obj.size[0] and
                event.pos[1] >= obj.pos[1] and
                event.pos[1] < obj.pos[1] + obj.size[1])
            {
                const bottom_right = obj.pos + obj.size - event.pos;
                const top_left = event.pos - obj.pos;
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
                captured_offset = event.pos - obj.pos;
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

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn layout(self: *const Window, handle: Handle, _: LayoutConstraint) ng.Vec2 {
        _ = self;

        const constraint = LayoutConstraint{
            .min_size = .{ 0, 0 },
            .max_size = .{ std.math.inf(f32), std.math.inf(f32) },
        };

        const obj = get(handle) catch return .{ 0, 0 };
        var pos = obj.pos + ng.Vec2{ 0, 26 };

        var iter = handle.children() catch return .{ 0, 0 };
        while (iter.next()) |child_handle| {
            var child_obj = get(child_handle) catch return obj.size;

            child_obj.pos = pos;

            const size = child_handle.layout(constraint);

            child_obj.size = size;

            pos[1] += size[1];
        }

        return .{ 0, 0 };
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const VBox = struct {
    pub fn layout(self: *const VBox, handle: Handle, constraint: LayoutConstraint) ng.Vec2 {
        _ = self;
        const obj = get(handle) catch return .{ 0, 0 };

        var pos = obj.pos + Vec2{ obj.padding.left, obj.padding.top };
        var max_size = ng.Vec2{ 0, 0 };

        var iter = obj.children();
        while (iter.next()) |child| {
            var child_obj = get(child) catch return max_size;

            child_obj.pos = pos;

            const size = child.layout(constraint);
            if (size[0] > max_size[0]) {
                max_size[0] = size[0];
            }

            child_obj.size = size;

            pos[1] += size[1];
            max_size[1] += size[1];
        }

        return max_size;
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const HBox = struct {
    pub fn layout(self: *const HBox, handle: Handle, constraint: LayoutConstraint) ng.Vec2 {
        _ = self;
        const obj = get(handle) catch return .{ 0, 0 };

        var pos = obj.pos + Vec2{ obj.padding.left, obj.padding.top };
        var max_size = ng.Vec2{ 0, 0 };

        var iter = obj.children();
        while (iter.next()) |child| {
            var child_obj = get(child) catch return max_size;

            child_obj.pos = pos;

            const size = child.layout(constraint);
            if (size[1] > max_size[1]) {
                max_size[1] = size[1];
            }

            child_obj.size = size;

            pos[0] += size[0];
            max_size[0] += size[0];
        }

        max_size += Vec2{ obj.padding.left, obj.padding.top };
        max_size += Vec2{ obj.padding.right, obj.padding.bottom };

        return max_size;
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const Text = struct {
    memory: []u8,
    text: []const u8,
    allocated: bool,

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn draw(self: Text, obj: *const Object) void {
        draw_text(
            obj.pos,
            self.text,
            20,
            .yellow,
        );
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn layout(self: *const Text, _: Handle, _: LayoutConstraint) ng.Vec2 {
        return ng.Vec2{ @as(f32, @floatFromInt(self.text.len)) * 12, 20 };
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const Button = struct {
    min_size: Vec2,
    background_color: Color = .@"dark grey",
    clicked: bool = false,

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn process_event(self: *Button, handle: Handle, event: ng.Event) bool {
        switch (event) {
            .mouse_down => |ev| return self.process_mouse_down(handle, ev),
            .mouse_up => |ev| return self.process_mouse_up(handle, ev),
            else => {},
        }
        return false;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    fn process_mouse_down(self: *Button, handle: Handle, event: ng.MouseEvent) bool {
        _ = self;
        _ = handle;
        _ = event;
        return true;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    fn process_mouse_up(self: *Button, handle: Handle, event: ng.MouseEvent) bool {
        _ = self;
        _ = handle;
        _ = event;
        return true;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn draw(self: Button, obj: *const Object) void {
        draw_rectangle(obj.pos, obj.size, self.background_color);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn layout(self: *const Button, handle: Handle, constraint: LayoutConstraint) ng.Vec2 {
        const obj = get(handle) catch return .{ 0, 0 };

        const internal_constraint = LayoutConstraint{
            .min_size = @min(self.min_size, constraint.min_size),
            .max_size = @max(self.min_size, constraint.max_size),
        };

        var size: ng.Vec2 = .{ 0, 0 };

        var iter = handle.children() catch return .{ 0, 0 };
        while (iter.next()) |child_handle| {
            var child_obj = get(child_handle) catch return obj.size;

            child_obj.pos = obj.pos;

            const child_size = child_handle.layout(internal_constraint);

            child_obj.size = child_size;

            size = @max(child_size, size);
        }

        return size;
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn find_object(phandle: Handle, ident: Ident) ?Handle {
    const parent = get(phandle) catch return null;
    var object = parent.first_child;
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

fn add_child_last(phandle: Handle, chandle: Handle) void {
    const parent = get(phandle) catch return;
    const child = get(chandle) catch return;

    if (parent.last_child) |last| {
        child.pred = last;
        child.succ = null;
        const last_obj = get(last) catch return;
        last_obj.succ = chandle;
        parent.last_child = chandle;
    } else {
        child.pred = null;
        child.succ = null;
        parent.first_child = chandle;
        parent.last_child = chandle;
    }
}

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
    if (first_window) |window| {
        dump_state_internal(window, 1);
    }
}

const lots_of_spaces = " " ** 256;

fn dump_state_internal(first_child: Handle, depth: usize) void {
    var child: ?Handle = first_child;
    while (child) |handle| {
        const obj = get(handle) catch return;
        if (obj.shown) {
            ng.debug_print("{s}", .{lots_of_spaces[0 .. depth * 2]});
            ng.debug_print("{s}:{}", .{
                @tagName(obj.data),
                @intFromEnum(handle),
            });
            ng.debug_print(" {d} {d}", .{
                obj.pos,
                obj.size,
            });
            switch (obj.data) {
                .window => {},
                .vbox => {},
                .hbox => {},
                .text => |text| {
                    ng.debug_print(" \"{}\" {}/{}", .{
                        std.zig.fmtEscapes(text.text),
                        text.text.len,
                        text.memory.len,
                    });
                },
                .button => {},
            }
            ng.debug_print("\n", .{});
            if (obj.first_child) |first| {
                dump_state_internal(first, depth + 1);
            }
        }
        child = obj.succ;
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

fn draw_rectangle(pos: Vec2, size: Vec2, col: Color) void {
    add_vertex(.{
        .pos = pos,
        .uv = .{ 0, 0 },
        .col = col,
    });
    add_vertex(.{
        .pos = pos + Vec2{ size[0], 0 },
        .uv = .{ 0, 0 },
        .col = col,
    });
    add_vertex(.{
        .pos = pos + Vec2{ 0, size[1] },
        .uv = .{ 0, 0 },
        .col = col,
    });
    add_vertex(.{
        .pos = pos + Vec2{ 0, size[1] },
        .uv = .{ 0, 0 },
        .col = col,
    });
    add_vertex(.{
        .pos = pos + Vec2{ size[0], 0 },
        .uv = .{ 0, 0 },
        .col = col,
    });
    add_vertex(.{
        .pos = pos + size,
        .uv = .{ 0, 0 },
        .col = col,
    });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn draw_text(pos: Vec2, text: []const u8, height: f32, col: Color) void {
    var p = pos;

    const scale = height / 20;

    for (text) |ch| {
        const cx: f32 = @floatFromInt(ch & 15);
        const cy: f32 = @floatFromInt(ch / 16);
        const cx1: f32 = (cx * 12) / (16 * 12);
        const cy1: f32 = (cy * 20) / (16 * 20);
        const cx2: f32 = ((cx + 1) * 12) / (16 * 12);
        const cy2: f32 = ((cy + 1) * 20) / (16 * 20);

        add_vertex(.{
            .pos = p,
            .uv = .{ cx1, cy1 },
            .col = col,
        });
        add_vertex(.{
            .pos = p + Vec2{ 12 * scale, 0 },
            .uv = .{ cx2, cy1 },
            .col = col,
        });
        add_vertex(.{
            .pos = p + Vec2{ 0, 20 * scale },
            .uv = .{ cx1, cy2 },
            .col = col,
        });
        add_vertex(.{
            .pos = p + Vec2{ 0, 20 * scale },
            .uv = .{ cx1, cy2 },
            .col = col,
        });
        add_vertex(.{
            .pos = p + Vec2{ 12 * scale, 0 },
            .uv = .{ cx2, cy1 },
            .col = col,
        });
        add_vertex(.{
            .pos = p + Vec2{ 12 * scale, 20 * scale },
            .uv = .{ cx2, cy2 },
            .col = col,
        });

        p += Vec2{ 12 * scale, 0 };
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
