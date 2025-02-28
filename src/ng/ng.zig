///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const video = @import("video.zig");
pub const audio = @import("audio.zig");
pub const event = @import("event.zig");
pub const api = @import("api.zig");
pub const time = @import("time.zig");
pub const math = @import("math.zig");
pub const color = @import("color.zig");
pub const pool = @import("pool.zig");
pub const debug_text = @import("debug_text.zig");
pub const logger = @import("logger.zig");
pub const ecs = @import("ecs.zig");
pub const ui = @import("ui.zig");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const lookup_symbols = api.lookup_symbols;
pub const lookup_using = api.lookup_using;

pub const create_window = video.create_window;
pub const create_shader = video.create_shader;
pub const create_buffer = video.create_buffer;
pub const create_pipeline = video.create_pipeline;
pub const create_binding = video.create_binding;
pub const create_image = video.create_image;
pub const create_sampler = video.create_sampler;
pub const use_cursor = video.use_cursor;
pub const Sampler2D = video.Sampler2D;
pub const Window = video.Window;
pub const WindowSize = video.WindowSize;
pub const Shader = video.Shader;
pub const Pipeline = video.Pipeline;
pub const Buffer = video.Buffer;
pub const Binding = video.Binding;
pub const RenderPass = video.RenderPass;

pub const debug_text_draw = debug_text.draw;

pub const Vec2 = math.Vec2;
pub const Vec3 = math.Vec3;
pub const Vec4 = math.Vec4;
pub const Mat4 = math.Mat4;
pub const mat4_identity = math.mat4_identity;
pub const ortho = math.ortho;
pub const mat4_mul = math.mat4_mul;
pub const mat4_invert = math.mat4_invert;
pub const mat4_transform = math.mat4_transform;
pub const mat4_transform2 = math.mat4_transform2;
pub const Camera2D = math.Camera2D;

pub const ui_render = ui.render;
pub const ui_begin_window = ui.begin_window;
pub const ui_end_window = ui.end_window;
pub const ui_begin_box = ui.begin_box;
pub const ui_end_box = ui.end_box;
pub const ui_format_text = ui.format_text;

pub const Color = color.Color;

pub const Event = event.Event;
pub const KeyEvent = event.KeyEvent;
pub const MoveEvent = event.MoveEvent;
pub const MouseEvent = event.MouseEvent;
pub const WheelEvent = event.WheelEvent;
pub const Key = event.Key;

pub const Pool = pool.Pool;

pub const Logger = logger.Logger;

pub const elapsed = time.elapsed;

pub const Entity = ecs.Entity;
pub const register_component = ecs.register_component;
pub const new = ecs.new;
pub const set = ecs.set;
pub const dump_ecs = ecs.dump_ecs;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

var prng: std.Random.DefaultPrng = undefined;

var event_queue: [256]Event = undefined;
var event_queue_count: usize = 0;
var event_queue_write_index: usize = 0;
var event_queue_read_index: usize = 0;

const log = Logger(.ng);

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub var key_pressed: [256]bool = undefined;
pub var key_down: [256]bool = undefined;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const InitOptions = struct {
    video: bool = false,
    audio: bool = false,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn init(options: InitOptions) !void {
    log.info("init", .{});

    @memset(&key_pressed, false);
    @memset(&key_down, false);

    if (options.video) {
        try video.init();
    }
    if (options.audio) {
        try audio.init();
    }

    time.init();
    const now = time.wallclock_us();

    prng = std.Random.DefaultPrng.init(now);

    ecs.init();
    try ui.init();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn deinit() void {
    log.info("deinit", .{});
    ui.deinit();
    ecs.deinit();
    time.deinit();
    audio.deinit();
    video.deinit();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn poll_event() ?Event {
    video.generate_events();

    while (event_queue_count > 0) {
        const ev = event_queue[event_queue_read_index];
        event_queue_read_index = (event_queue_read_index + 1) % event_queue.len;
        event_queue_count -= 1;

        if (ui.filter_event(ev)) |evn|
        {
            return evn;
        }
    }
    return null;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn send_event(ev: Event) void {
    if (event_queue_count < event_queue.len) {
        event_queue[event_queue_write_index] = ev;
        event_queue_write_index = (event_queue_write_index + 1) % event_queue.len;
        event_queue_count += 1;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn is_key_down(key: Key) bool {
    if (ui.is_hover()) return false;
    return key_down[@intFromEnum(key)];
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn rand() f32 {
    const random = prng.random();
    return random.float(f32);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn rand_u8() u8 {
    const random = prng.random();
    return random.int(u8);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn rand_color() Color {
    return Color.from_rgb_i(rand_u8(), rand_u8(), rand_u8());
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn as_bytes(data: anytype) []u8 {
    const T = @TypeOf(data);
    const ti = @typeInfo(T);

    switch (ti) {
        .pointer => |pointer| {
            switch (pointer.size) {
                .one => {
                    const cti = @typeInfo(pointer.child);
                    if (cti == .pointer) {
                        @compileError("Cannot as_bytes " ++ @typeName(T));
                    }
                    const ptr = std.mem.asBytes(data);
                    var block: []u8 = undefined;
                    block.ptr = @constCast(ptr);
                    block.len = @sizeOf(pointer.child);
                    return block;
                },
                .slice => {
                    var block: []u8 = undefined;
                    block.ptr = @constCast(@ptrCast(data.ptr));
                    block.len = @sizeOf(pointer.child) * data.len;
                    return block;
                },
                else => {
                    @compileError("Cannot as_bytes " ++ @typeName(T));
                },
            }
        },
        else => {
            @compileError("Cannot as_bytes " ++ @typeName(T));
        },
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "as_bytes" {
    {
        const a: u8 = 0x12;
        const b = as_bytes(&a);
        try std.testing.expectEqualSlices(u8, &[_]u8{0x12}, b);
    }

    {
        const a: u16 = 0x1234;
        const b = as_bytes(&a);
        try std.testing.expectEqualSlices(u8, &[_]u8{ 0x34, 0x12 }, b);
    }

    {
        const a: u32 = 0x12345678;
        const b = as_bytes(&a);
        try std.testing.expectEqualSlices(u8, &[_]u8{ 0x78, 0x56, 0x34, 0x12 }, b);
    }

    {
        const a: u64 = 0x12345678_9abcdef0;
        const b = as_bytes(&a);
        try std.testing.expectEqualSlices(u8, &[_]u8{
            0xf0,
            0xde,
            0xbc,
            0x9a,
            0x78,
            0x56,
            0x34,
            0x12,
        }, b);
    }

    {
        const a: f32 = 3.14;
        const b = as_bytes(&a);
        try std.testing.expectEqualSlices(u8, &[_]u8{ 0xc3, 0xf5, 0x48, 0x40 }, b);
    }

    {
        const a: f64 = 3.14;
        const b = as_bytes(&a);
        try std.testing.expectEqualSlices(u8, &[_]u8{
            0x1f,
            0x85,
            0xeb,
            0x51,
            0xb8,
            0x1e,
            0x09,
            0x40,
        }, b);
    }

    {
        const a: [3]u8 = .{ 1, 2, 3 };
        const b = as_bytes(&a);
        try std.testing.expectEqualSlices(u8, &[_]u8{ 0x01, 0x02, 0x03 }, b);
    }

    {
        const a: [3]f32 = .{ 1, 2, 3 };
        const b = as_bytes(&a);
        try std.testing.expectEqualSlices(u8, &[_]u8{
            0x00,
            0x00,
            0x80,
            0x3F,
            0x00,
            0x00,
            0x00,
            0x40,
            0x00,
            0x00,
            0x40,
            0x40,
        }, b);
    }

    {
        const Point = extern struct { x: f32, y: f32, c: u32 };
        const a: [5]Point = .{
            .{ .x = 1, .y = 2, .c = 3 },
            .{ .x = 4, .y = 5, .c = 6 },
            .{ .x = 7, .y = 8, .c = 9 },
            .{ .x = 10, .y = 11, .c = 12 },
            .{ .x = 13, .y = 14, .c = 15 },
        };
        var c: usize = 1;
        c += 2;
        const b = as_bytes(a[0..c]);
        try std.testing.expectEqualSlices(u8, &[_]u8{
            0, 0, 128, 63, 0, 0, 0,   64, 3, 0, 0, 0,
            0, 0, 128, 64, 0, 0, 160, 64, 6, 0, 0, 0,
            0, 0, 224, 64, 0, 0, 0,   65, 9, 0, 0, 0,
        }, b);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn make_uniform_slots(comptime Uniforms: type) type {
    var fields: [16]std.builtin.Type.EnumField = undefined;

    var num_fields: usize = 0;

    inline for (std.meta.fields(Uniforms)) |field| {
        fields[num_fields].value = num_fields;
        fields[num_fields].name = field.name;
        num_fields += 1;
    }

    const info = std.builtin.Type{ .@"enum" = .{
        .tag_type = u32,
        .fields = fields[0..num_fields],
        .decls = &.{},
        .is_exhaustive = true,
    } };

    return @Type(info);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

var last_now: u64 = 0;

pub fn start_frame() f32 {
    const now = time.elapsed_us();
    const delta = now - last_now;
    last_now = now;

    return @as(f32, @floatFromInt(delta)) / 1e6;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn end_frame() void {
    @memset(&key_pressed, false);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn debug_clear(window: video.Window) void {
    const size = window.get_size();
    debug_text.reset(@intFromFloat(size.width), @intFromFloat(size.height));
}

pub fn debug_puts(str: []const u8) void {
    debug_text.puts(str);
}

pub fn debug_print(comptime fmt: []const u8, args: anytype) void {
    debug_text.print(fmt, args);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
