///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");
const ng = @import("ng");

pub const Window = @import("ui/Window.zig");
pub const VBox = @import("ui/VBox.zig");
pub const HBox = @import("ui/HBox.zig");
pub const Text = @import("ui/Text.zig");
pub const Button = @import("ui/Button.zig");

const Vec2 = ng.Vec2;
const Color = ng.Color;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const log = ng.Logger(.ng_ui);

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
pub var allocator: std.mem.Allocator = undefined;

var all_objects: std.ArrayListUnmanaged(Object) = .empty;
var used_objects: std.ArrayListUnmanaged(bool) = .empty;
var generations: std.ArrayListUnmanaged(HandleGeneration) = .empty;
var recycled: std.ArrayListUnmanaged(HandleIndex) = .empty;

var init_required: bool = false;

var build_stack: [16]Handle = undefined;
var build_stack_index: usize = 0;

pub var first_window: ?Handle = null;
pub var last_window: ?Handle = null;

var last_mouse: ng.Vec2 = .{ 0, 0 };

pub var over: ?Handle = null;
pub var over_window: ?Handle = null;
pub var captured_mouse: ?Handle = null;
pub var window_resizing: ResizingMode = .none;
pub var captured_offset: Vec2 = .{ 0, 0 };
pub var app_captured_mouse: bool = false;

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

    if (!init_required) {
        ui_shader.delete();
        ui_image.delete();
        ui_sampler.delete();

        for (all_objects.items) |*obj| {
            obj.deinit();
        }

        all_objects.deinit(allocator);
        used_objects.deinit(allocator);
        generations.deinit(allocator);
        recycled.deinit(allocator);
    }

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

    if (first_window) |window| {
        var iter = ui_walk(window);
        while (iter.next()) |handle| {
            const obj = get(handle) catch return;
            if (obj.active) {
                obj.shown = obj.active;
                obj.active = false;
            } else {
                obj.shown = false;
            }
        }
    }

    var object = first_window;
    while (object) |handle| {
        var obj = get(handle) catch return;
        object = obj.succ;

        obj.fit_to_display(display_size);

        _ = obj.layout(handle, .{});
    }

    object = last_window;
    while (object) |handle| {
        const obj = get(handle) catch return;
        object = obj.pred;

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
    }

    if (false) {
        dump_state("Render");
    }

    object = first_window;
    while (object) |handle| {
        const obj = get(handle) catch return;
        object = obj.succ;

        delete_not_shown_in(handle);
    }

    if (true) {
        dump_state("Render");
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

fn delete_not_shown_in(phandle: Handle) void {
    const parent = get(phandle) catch return;
    var child: ?Handle = parent.first_child;
    while (child) |chandle| {
        const obj = get(chandle) catch return;
        child = obj.succ;

        if (obj.first_child) |_| {
            delete_not_shown_in(chandle);
        }

        if (obj.shown == false) {
            remove_child(phandle, chandle);
            delete(chandle);
        }
    }
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

pub fn add_vertex(vertex: DebugTextVertex) void {
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

pub fn add_scissor(pos: ng.Vec2, size: ng.Vec2) void {
    if (next_command < commands.len) {
        commands[next_command] = .{ .scissor = .{ .pos = pos, .size = size } };
        next_command += 1;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn push_build_stack(handle: Handle) void {
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

pub fn pop_build_stack() void {
    if (build_stack_index > 0) {
        build_stack_index -= 1;
    } else {
        log.err("Build stack underflow", .{});
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn build_stack_empty() bool {
    return build_stack_index == 0;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn top_build_stack() Handle {
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

pub const Ident = struct {
    addr: usize,
    unique: usize,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const HandleIndex = u24;
const HandleGeneration = u8;

pub const Handle = packed struct(u32) {
    idx: HandleIndex,
    gen: HandleGeneration,

    pub fn children(self: Handle) !Object.ObjectChildrenIterator {
        const obj = try get(self);
        return obj.children();
    }

    pub fn layout(self: Handle, constraint: LayoutConstraint) ng.Vec2 {
        const obj = get(self) catch return .{ 0, 0 };
        return obj.layout(self, constraint);
    }

    pub fn format(self: Handle, _: anytype, _: anytype, writer: anytype) !void {
        try writer.print("#{d}:{d}", .{ self.gen, self.idx });
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

pub const Padding = struct {
    left: f32 = 0,
    top: f32 = 0,
    right: f32 = 0,
    bottom: f32 = 0,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Object = struct {
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

    pub fn deinit(self: *Object) void {
        switch (self.data) {
            .window => |*obj| obj.deinit(),
            .vbox => |*obj| obj.deinit(),
            .hbox => |*obj| obj.deinit(),
            .text => |*obj| obj.deinit(),
            .button => |*obj| obj.deinit(),
        }
    }

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
        var iter = self.children();
        while (iter.next()) |child| {
            const obj = get(child) catch continue;
            if (@reduce(.And, last_mouse > obj.pos) and
                @reduce(.And, last_mouse < obj.pos + obj.size))
            {
                if (obj.process_event(child, event)) {
                    return true;
                }
            }
        }

        switch (self.data) {
            .window => |*win| return win.process_event(handle, event),
            .button => |*button| return button.process_event(handle, event),
            .vbox => |*vbox| return vbox.process_event(handle, event),
            .hbox => |*hbox| return hbox.process_event(handle, event),
            .text => |*text| return text.process_event(handle, event),
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

pub const LayoutConstraint = struct {
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

pub fn find_object(phandle: Handle, ident: Ident) ?Handle {
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

pub fn remove_child(phandle: Handle, chandle: Handle) void {
    const parent = get(phandle) catch return;
    const child = get(chandle) catch return;

    if (child.pred) |pred| {
        const pred_obj = get(pred) catch return;
        pred_obj.succ = child.succ;
    } else {
        parent.first_child = child.succ;
    }
    if (child.succ) |succ| {
        const succ_obj = get(succ) catch return;
        succ_obj.pred = child.pred;
    } else {
        parent.last_child = child.pred;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn add_child_last(phandle: Handle, chandle: Handle) void {
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

pub fn move_child_last(phandle: Handle, chandle: Handle) void {
    remove_child(phandle, chandle);
    add_child_last(phandle, chandle);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn find_window(ident: Ident) ?Handle {
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

pub fn move_to_top(handle: Handle) void {
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

const max_ui_iterator_depth = 32;

const Walker = struct {
    index: usize,
    depth: usize = 0,
    stack: [max_ui_iterator_depth]?Handle = undefined,

    pub fn next(self: *Walker) ?Handle {
        while (self.index > 0) {
            if (self.stack[self.index - 1]) |handle| {
                self.depth = self.index;
                const obj = get(handle) catch {
                    self.index = 0;
                    return null;
                };
                if (obj.succ) |succ| {
                    self.stack[self.index - 1] = succ;
                } else {
                    self.stack[self.index - 1] = null;
                }
                if (obj.first_child) |first| {
                    if (self.index < max_ui_iterator_depth) {
                        self.stack[self.index] = first;
                        self.index += 1;
                    } else {
                        log.err("UI_Iterator overflow", .{});
                        self.index = 0;
                        return null;
                    }
                }
                return handle;
            } else {
                self.index -= 1;
            }
        }
        return null;
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn ui_walk(handle: Handle) Walker {
    return .{
        .stack = .{handle} ** max_ui_iterator_depth,
        .index = 1,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn dump_state(label: []const u8) void {
    ng.debug_print("{s} : {?} : {?} : {?} : {} : {?} : {?}\n", .{
        label,
        first_window,
        last_window,
        captured_mouse,
        app_captured_mouse,
        over_window,
        over,
    });

    const lots_of_spaces = " " ** 256;

    if (first_window) |window| {
        var iter = ui_walk(window);
        while (iter.next()) |handle| {
            const obj = get(handle) catch return;
            ng.debug_print("{s}", .{lots_of_spaces[0 .. iter.depth * 2]});
            ng.debug_print("{} {s}", .{
                handle,
                @tagName(obj.data),
            });
            ng.debug_print(" {d} {d} {s}", .{
                obj.pos,
                obj.size,
                if (obj.shown) "shown" else "hidden",
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
                .button => |button| {
                    ng.debug_print(" {} {}", .{ button.clicked, button.pressed });
                },
            }
            ng.debug_print("\n", .{});
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn new() !Handle {
    if (recycled.pop()) |index| {
        const gen = generations.items[index];
        used_objects.items[index] = true;
        return .{ .idx = index, .gen = gen };
    } else {
        const index = all_objects.items.len;

        try all_objects.append(allocator, undefined);
        try used_objects.append(allocator, true);
        try generations.append(allocator, 0);

        return .{ .idx = @intCast(index), .gen = 0 };
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn delete(handle: Handle) void {
    const obj = get(handle) catch return;
    obj.deinit();

    used_objects.items[handle.idx] = false;
    generations.items[handle.idx] +%= 1;
    recycled.append(allocator, handle.idx) catch return;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn get(handle: Handle) !*Object {
    if (handle.idx < used_objects.items.len and
        used_objects.items[handle.idx] and
        generations.items[handle.idx] == handle.gen)
    {
        return &all_objects.items[handle.idx];
    }

    if (handle.idx >= used_objects.items.len) {
        log.fatal("Get {} invalid index", .{handle});
    } else if (!used_objects.items[handle.idx]) {
        log.fatal("Get {} invalid not used", .{handle});
    } else if (generations.items[handle.idx] != handle.gen) {
        log.fatal("Get {} invalid generation", .{handle});
    } else {
        log.fatal("Get {} invalid", .{handle});
    }
    log.callstack();
    return error.InvalidHandle;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn is_over_window() bool {
    if (over_window) |handle| {
        _ = get(handle) catch {
            over_window = null;
            over = null;
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
            // FIXME: Not convinced that this should not just be a break
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

    switch (event) {
        .mouse_move => |ev| {
            last_mouse = ev.pos;
        },
        else => {},
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

pub fn draw_rectangle(pos: Vec2, size: Vec2, col: Color) void {
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

pub fn draw_text(pos: Vec2, text: []const u8, height: f32, col: Color) void {
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
