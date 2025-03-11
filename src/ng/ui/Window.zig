///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");
const ng = @import("ng");

const ui = ng.ui;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const Window = @This();

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

title: []const u8,
min_size: ng.Vec2,
max_size: ng.Vec2,
background_color: ng.Color,
title_bar_color: ng.Color,
title_bar_height: f32,
title_text_color: ng.Color,
resize_handle_color: ng.Color,
resize_handle_size: f32,
resize_border_size: f32,

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const Object = ng.ui.Object;
const Handle = ng.ui.Handle;
const LayoutConstraint = ng.ui.LayoutConstraint;

const log = ng.ui.log;

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
    padding: ui.Padding = .{ .left = 0, .top = 0, .right = 0, .bottom = 0 },
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

pub noinline fn begin_window(options: BeginWindowOptions) void {
    const ident = ui.Ident{ .addr = @returnAddress(), .unique = options.unique };

    while (!ui.build_stack_empty()) {
        log.err("Build stack not empty", .{});
        ui.pop_build_stack();
    }

    if (ui.find_window(ident)) |handle| {
        var window = ui.get(handle) catch return;
        if (window.shown == false) {
            ui.move_to_top(handle);
        }
        window.active = true;

        ui.push_build_stack(handle);
    } else {
        const handle = ui.new() catch |err| {
            log.err("begin_window {}", .{err});
            return;
        };

        const object = ui.get(handle) catch return;

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

        ui.move_to_top(handle);

        ui.push_build_stack(handle);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn end_window() void {
    ui.pop_build_stack();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn deinit(_: *const Window) void {}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn draw(self: Window, obj: *const Object) void {
    ng.ui.add_scissor(obj.pos, obj.size);
    ng.ui.draw_rectangle(obj.pos, obj.size, self.background_color);

    if (false) // let's not draw this anymore
    {
        ng.ui.add_vertex(.{
            .pos = obj.pos + obj.size - ng.Vec2{ 0, self.resize_handle_size },
            .uv = .{ 0, 0 },
            .col = self.resize_handle_color,
        });

        ng.ui.add_vertex(.{
            .pos = obj.pos + obj.size,
            .uv = .{ 0, 0 },
            .col = self.resize_handle_color,
        });

        ng.ui.add_vertex(.{
            .pos = obj.pos + obj.size - ng.Vec2{ self.resize_handle_size, 0 },
            .uv = .{ 0, 0 },
            .col = self.resize_handle_color,
        });
    }

    ng.ui.draw_rectangle(
        obj.pos,
        ng.Vec2{ obj.size[0], self.title_bar_height },
        self.title_bar_color,
    );

    ng.ui.draw_text(
        obj.pos + ng.Vec2{ 4, 4 },
        self.title,
        self.title_bar_height - 8,
        self.title_text_color,
    );
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

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

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn process_mouse_move(self: *Window, handle: Handle, event: ng.MoveEvent) bool {
    if (ng.ui.captured_mouse == handle) {
        var obj = ng.ui.get(handle) catch return false;
        switch (ng.ui.window_resizing) {
            .none => {
                obj.pos = event.pos - ng.ui.captured_offset;
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
                const new_pos = event.pos[1] - ng.ui.captured_offset[1];
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
                const new_pos = event.pos[0] - ng.ui.captured_offset[0];
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
        const obj = ng.ui.get(handle) catch return false;
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
            ng.ui.over_window = handle;
            return true;
        }
        ng.ui.over_window = null;
        ng.ui.over = null;
    }
    return false;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn process_mouse_down(self: Window, handle: Handle, event: ng.MouseEvent) bool {
    const obj = ng.ui.get(handle) catch return false;
    if (event.button == .left) {
        if (event.pos[0] >= obj.pos[0] and
            event.pos[0] < obj.pos[0] + obj.size[0] and
            event.pos[1] >= obj.pos[1] and
            event.pos[1] < obj.pos[1] + obj.size[1])
        {
            const bottom_right = obj.pos + obj.size - event.pos;
            const top_left = event.pos - obj.pos;
            if (bottom_right[0] + bottom_right[1] < self.resize_handle_size) {
                ng.ui.window_resizing = .resize;
            } else if (bottom_right[0] < self.resize_border_size) {
                ng.ui.window_resizing = .right;
            } else if (top_left[0] < self.resize_border_size) {
                ng.ui.window_resizing = .left;
            } else if (bottom_right[1] < self.resize_border_size) {
                ng.ui.window_resizing = .bottom;
            } else if (top_left[1] < self.resize_border_size) {
                ng.ui.window_resizing = .top;
            } else {
                ng.ui.window_resizing = .none;
            }
            ng.ui.captured_mouse = handle;
            ng.ui.captured_offset = event.pos - obj.pos;
            ng.ui.move_to_top(handle);
            return true;
        }
    }
    return false;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn process_mouse_up(self: Window, handle: Handle, event: ng.MouseEvent) bool {
    _ = self;
    if (event.button == .left) {
        if (ng.ui.captured_mouse == handle) {
            ng.ui.captured_mouse = null;
            return true;
        }
    }
    return false;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn layout(self: *const Window, handle: Handle, _: LayoutConstraint) ng.Vec2 {
    _ = self;

    const constraint = LayoutConstraint{
        .min_size = .{ 0, 0 },
        .max_size = .{ std.math.inf(f32), std.math.inf(f32) },
    };

    const obj = ng.ui.get(handle) catch return .{ 0, 0 };
    var pos = obj.pos + ng.Vec2{ 0, 26 };

    var iter = handle.children() catch return .{ 0, 0 };
    while (iter.next()) |child_handle| {
        var child_obj = ng.ui.get(child_handle) catch return obj.size;

        child_obj.pos = pos;

        const size = child_handle.layout(constraint);

        child_obj.size = size;

        pos[1] += size[1];
    }

    return .{ 0, 0 };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
