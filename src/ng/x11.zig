///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const c = @cImport({
    @cInclude("X11/Xlib.h");
    @cInclude("X11/Xutil.h");
    @cInclude("GL/glx.h");
});

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const ng = @import("ng.zig");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const video = @import("video.zig");
const Pool = @import("pool.zig").Pool;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const API = struct {
    XCloseDisplay: *const fn (*Display) callconv(.c) void,
    XCreateColormap: *const fn (*Display, Window, *c.Visual, u32) callconv(.c) Colormap,
    XCreateWindow: *const fn (*Display, Window, u32, u32, u32, u32, u32, c_int, u32, *c.Visual, CWValueMask, *XSetWindowAttributes) callconv(.c) Window,
    XDefaultScreen: *const fn (*Display) callconv(.c) u32,
    XDefaultScreenOfDisplay: *const fn (*Display) callconv(.c) *Screen,
    XDestroyWindow: *const fn (*Display, Window) callconv(.c) void,
    XInternAtom: *const fn (*Display, [*:0]const u8, bool) callconv(.c) Atom,
    XMapRaised: *const fn (*Display, Window) callconv(.c) void,
    XNextEvent: *const fn (*Display, *c.XEvent) callconv(.c) void,
    XOpenDisplay: *const fn ([*c]const u8) callconv(.c) *Display,
    XPending: *const fn (*Display, *c.XEvent) callconv(.c) i32,
    XRootWindowOfScreen: *const fn (*Screen) callconv(.c) Window,
    XSetWMProtocols: *const fn (*Display, Window, [*c]XID, u32) callconv(.c) void,
    XStoreName: *const fn (*Display, Window, [*:0]const u8) callconv(.c) void,
    XSync: *const fn (*Display, bool) callconv(.c) void,
    glXChooseFBConfig: *const fn (*Display, u32, [*c]const u32, *u32) callconv(.c) [*c]c.GLXFBConfig,
    glXGetVisualFromFBConfig: *const fn (*Display, c.GLXFBConfig) callconv(.c) ?*c.XVisualInfo,
    glXQueryVersion: *const fn (*Display, *u32, *u32) callconv(.c) bool,
    glXGetFBConfigAttrib: *const fn (*Display, c.GLXFBConfig, u32, *u32) callconv(.c) void,
    glXCreateContextAttribsARB: ?*const fn (*Display, c.GLXFBConfig, u32, u32, [*c]const u32) callconv(.c) ?*GLXContext,
    glXGetProcAddressARB: *const fn ([*:0]const u8) callconv(.c) ?*anyopaque,
    glXMakeCurrent: *const fn (*Display, Window, *GLXContext) callconv(.c) void,
    glXSwapBuffers: *const fn (*Display, Window) callconv(.c) void,
    glClearColor: *const fn (f32, f32, f32, f32) callconv(.c) void,
    glClear: *const fn (u32) callconv(.c) void,
    glViewport: *const fn (u32, u32, u32, u32) callconv(.c) void,
    glScissor: *const fn (u32, u32, u32, u32) callconv(.c) void,
    glCreateShader: *const fn (u32) callconv(.c) u32,
    glDeleteShader: *const fn (u32) callconv(.c) void,
    glShaderSource: *const fn (u32, u32, *const [*:0]const u8, ?*u32) callconv(.c) void,
    glCompileShader: *const fn (u32) callconv(.c) void,
    glGetShaderiv: *const fn (u32, u32, *u32) callconv(.c) void,
    glGetShaderInfoLog: *const fn (u32, u32, *u32, [*c]u8) callconv(.c) void,
    glCreateProgram: *const fn () callconv(.c) u32,
    glGetProgramInfoLog: *const fn (u32, u32, *u32, [*c]u8) callconv(.c) void,
    glAttachShader: *const fn (u32, u32) callconv(.c) void,
    glDeleteProgram: *const fn (u32) callconv(.c) void,
    glLinkProgram: *const fn (u32) callconv(.c) void,
    glGetProgramiv: *const fn (u32, u32, *u32) callconv(.c) void,
    glGenBuffers: *const fn (u32, [*c]u32) callconv (.c) void,
    glDeleteBuffers: *const fn (u32, [*c]u32) callconv (.c) void,
    glBindBuffer: *const fn (u32, u32) callconv (.c) void,
    glBufferData: *const fn (u32, usize, *anyopaque, u32) callconv (.c) void,
};

var api: API = undefined;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const XID = c_ulong;

const Display = anyopaque;
const Screen = anyopaque;
const GLXContext = anyopaque;
const Window = XID;
const Pixmap = XID;
const Colormap = XID;
const Cursor = XID;
const Atom = XID;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

var display: *Display = undefined;
var screen: *Screen = undefined;
var screen_id: u32 = undefined;
var root_window: Window = undefined;
var glx_ctx: *GLXContext = undefined;
var window: Window = undefined;

var window_width: f32 = 0;
var window_height: f32 = 0;

var shader_pool: Pool(GL_Shader, 256) = .{};
var buffer_pool: Pool(GL_Buffer, 1024) = .{};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

var WM_DELETE_WINDOW: Atom = undefined;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn init() !video.Platform {
    api = try ng.lookup_symbols(API, &.{
        "libX11.so",
        "libxcb.so",
        "libGLX.so",
        "libGL.so",
    });

    display = api.XOpenDisplay(null);

    screen = api.XDefaultScreenOfDisplay(display);
    screen_id = api.XDefaultScreen(display);

    root_window = api.XRootWindowOfScreen(screen);

    WM_DELETE_WINDOW = api.XInternAtom(display, "WM_DELETE_WINDOW", false);

    var glx_major: u32 = 0;
    var glx_minor: u32 = 0;
    if (api.glXQueryVersion(display, &glx_major, &glx_minor)) {
        if (glx_major == 1 and glx_minor < 3) {
            return error.GLXVersionInvalid;
        }
        // std.debug.print("GLX v{}.{}\n", .{ glx_major, glx_minor });
    }

    ng.lookup_using(API, &api, api.glXGetProcAddressARB);

    if (api.glXCreateContextAttribsARB == null) {
        return error.MissingSymbol_glxXCreateContextAttribsARB;
    }

    return .{
        .create_window = create_window,
        .close_window = close_window,
        .acquire_command_buffer = acquire_command_buffer,
        .acquire_swapchain_texture = acquire_swapchain_texture,
        .begin_render_pass = begin_render_pass,
        .end_render_pass = end_render_pass,
        .submit_command_buffer = submit_command_buffer,
        .deinit = deinit,
        .poll_event = poll_event,
        .create_shader = create_shader,
        .delete_shader = delete_shader,
        .create_buffer = create_buffer,
        .delete_buffer = delete_buffer,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn deinit() void {
    api.XCloseDisplay(display);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn create_window(options: video.CreateWindowOptions) video.VideoError!video.Window {
    const glx_attrs = [_]u32{
        GLX_X_RENDERABLE,   c.True,
        GLX_DRAWABLE_TYPE,  GLX_WINDOW_BIT,
        GLX_RENDER_TYPE,    GLX_RGBA_BIT,
        GLX_X_VISUAL_TYPE,  GLX_TRUE_COLOR,
        GLX_RED_SIZE,       8,
        GLX_GREEN_SIZE,     8,
        GLX_BLUE_SIZE,      8,
        GLX_ALPHA_SIZE,     8,
        GLX_DEPTH_SIZE,     24,
        GLX_STENCIL_SIZE,   8,
        GLX_DOUBLEBUFFER,   c.True,
        GLX_SAMPLE_BUFFERS, 1,
        GLX_SAMPLES,        4,
        0,
    };

    var fb_count: u32 = undefined;
    const fbc = api.glXChooseFBConfig(display, screen_id, &glx_attrs, &fb_count) orelse {
        std.debug.print("glXChooseFBConfig failed\n", .{});
        return error.CannotOpenWindow;
    };

    const fb_config = fbc[0];

    const vi = api.glXGetVisualFromFBConfig(display, fb_config) orelse {
        std.debug.print("glXGetVisualFromFBConfig failed\n", .{});
        return error.CannotOpenWindow;
    };

    const colormap = api.XCreateColormap(display, root_window, vi.visual, c.AllocNone);

    var cw_attributes: XSetWindowAttributes = undefined;
    var cw_value_mask: CWValueMask = .{};

    cw_attributes.event_mask = .{
        .key_press = true,
        .key_release = true,
        .pointer_motion = true,
        .button_press = true,
        .button_release = true,
        .enter_window = true,
        .leave_window = true,
        .focus_change = true,
        .exposure = true,
        .structure_notify = true,
    };
    cw_value_mask.event_mask = true;

    cw_attributes.colormap = colormap;
    cw_value_mask.colormap = true;

    cw_attributes.background_pixmap = 0;
    cw_value_mask.background_pixmap = true;

    cw_attributes.background_pixel = 0;
    cw_value_mask.background_pixel = true;

    cw_attributes.border_pixel = 0;
    cw_value_mask.border_pixel = true;

    window_width = @floatFromInt(options.width orelse 1920);
    window_height = @floatFromInt(options.height orelse 1920);

    window = api.XCreateWindow(
        display,
        root_window,
        0,
        0,
        options.width orelse 1920,
        options.height orelse 1080,
        0,
        vi.depth,
        c.InputOutput,
        vi.visual,
        cw_value_mask,
        &cw_attributes,
    );

    const context_attribs = [_]u32{
        GLX_CONTEXT_MAJOR_VERSION_ARB, options.gl_major_version,
        GLX_CONTEXT_MINOR_VERSION_ARB, options.gl_minor_version,
        GLX_CONTEXT_FLAGS_ARB,         GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB,
        0,
    };

    glx_ctx = api.glXCreateContextAttribsARB.?(
        display,
        fb_config,
        0,
        c.True,
        &context_attribs,
    ) orelse {
        std.debug.print("glXCreateContextAttribsARB failed\n", .{});
        return error.CannotOpenWindow;
    };

    api.XSync(display, false);

    api.glXMakeCurrent(display, window, glx_ctx);

    api.XSetWMProtocols(display, window, &WM_DELETE_WINDOW, 1);

    api.XMapRaised(display, window);

    if (options.name) |name| {
        api.XStoreName(display, window, name);
    }

    return .{
        .handle = window,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn close_window(a_window: video.Window) void {
    api.XDestroyWindow(display, a_window.handle);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const GLX_RGBA = 4;
const GLX_DOUBLEBUFFER = 5;
const GLX_RED_SIZE = 8;
const GLX_GREEN_SIZE = 9;
const GLX_BLUE_SIZE = 10;
const GLX_ALPHA_SIZE = 12;
const GLX_DEPTH_SIZE = 12;
const GLX_STENCIL_SIZE = 13;
const GLX_X_VISUAL_TYPE = 0x22;
const GLX_TRUE_COLOR = 0x8002;
const GLX_DRAWABLE_TYPE = 0x8010;
const GLX_RENDER_TYPE = 0x8011;
const GLX_X_RENDERABLE = 0x8012;
const GLX_SAMPLE_BUFFERS = 0x186a0;
const GLX_SAMPLES = 0x186a1;
const GLX_WINDOW_BIT = 1;
const GLX_RGBA_BIT = 1;
const GLX_CONTEXT_MAJOR_VERSION_ARB = 0x2091;
const GLX_CONTEXT_MINOR_VERSION_ARB = 0x2092;
const GLX_CONTEXT_FLAGS_ARB = 0x2094;
const GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB = 2;
const GLX_CONTEXT_DEBUG_BIT_ARB = 1;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const GL_COLOR_BUFFER_BIT = 0x4000;

const GL_FRAGMENT_SHADER = 0x8b30;
const GL_VERTEX_SHADER = 0x8b31;
const GL_COMPILE_STATUS = 0x8b81;
const GL_LINK_STATUS = 0x8b82;

const GL_ARRAY_BUFFER = 0x8892;
const GL_ELEMENT_ARRAY_BUFFER = 0x8893;

const GL_STREAM_DRAW = 0x88e0;
const GL_STREAM_READ = 0x88e1;
const GL_STREAM_COPY = 0x88e2;
const GL_STATIC_DRAW = 0x88e4;
const GL_STATIC_READ = 0x88e5;
const GL_STATIC_COPY = 0x88e6;
const GL_DYNAMIC_DRAW = 0x88e8;
const GL_DYNAMIC_READ = 0x88e9;
const GL_DYNAMIC_COPY = 0x88ea;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const CWValueMask = packed struct(u32) {
    background_pixmap: bool = false,
    background_pixel: bool = false,
    border_pixmap: bool = false,
    border_pixel: bool = false,
    bit_gravity: bool = false,
    win_gravity: bool = false,
    backing_store: bool = false,
    backing_planes: bool = false,
    backing_pixel: bool = false,
    override_redirect: bool = false,
    save_under: bool = false,
    event_mask: bool = false,
    dont_propagate: bool = false,
    colormap: bool = false,
    cursor: bool = false,
    _0: u17 = 0,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const XSetWindowAttributes = extern struct {
    background_pixmap: Pixmap,
    background_pixel: c_ulong,
    border_pixmap: Pixmap,
    border_pixel: c_ulong,
    bit_gravity: i32,
    win_gravity: i32,
    backing_store: i32,
    backing_planes: c_ulong,
    backing_pixel: c_ulong,
    save_under: bool,
    event_mask: EventMask,
    do_not_propagate_mask: c_long,
    override_redirect: bool,
    colormap: Colormap,
    cursor: Cursor,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const EventMask = packed struct(c_long) {
    key_press: bool = false,
    key_release: bool = false,
    button_press: bool = false,
    button_release: bool = false,
    enter_window: bool = false,
    leave_window: bool = false,
    pointer_motion: bool = false,
    pointer_motion_hint: bool = false,
    button1_motion: bool = false,
    button2_motion: bool = false,
    button3_motion: bool = false,
    button4_motion: bool = false,
    button5_motion: bool = false,
    button_motion: bool = false,
    keymap_state: bool = false,
    exposure: bool = false,
    visibility_change: bool = false,
    structure_notify: bool = false,
    resize_redirect: bool = false,
    substructure_notify: bool = false,
    substructure_redirect: bool = false,
    focus_change: bool = false,
    property_change: bool = false,
    colormap_change: bool = false,
    owner_grab_button: bool = false,
    _0: u39 = 0,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn poll_event() ?ng.Event {
    var ev: c.XEvent = undefined;

    while (api.XPending(display, &ev) > 0) {
        api.XNextEvent(display, &ev);

        switch (ev.type) {
            c.KeyPress => {
                if (process_key_press(ev.xkey)) |nev| return nev;
            },
            c.KeyRelease => {
                if (process_key_release(ev.xkey)) |nev| return nev;
            },
            c.ButtonPress => {
                if (process_button_press(ev.xbutton)) |nev| return nev;
            },
            c.ButtonRelease => {
                if (process_button_release(ev.xbutton)) |nev| return nev;
            },
            c.EnterNotify => {
                if (process_enter_window(ev.xcrossing)) |nev| return nev;
            },
            c.LeaveNotify => {
                if (process_leave_window(ev.xcrossing)) |nev| return nev;
            },
            c.FocusIn => {
                if (process_focus_in(ev.xfocus)) |nev| return nev;
            },
            c.FocusOut => {
                if (process_focus_out(ev.xfocus)) |nev| return nev;
            },
            c.MotionNotify => {
                if (process_motion_notify(ev.xmotion)) |nev| return nev;
            },
            c.ClientMessage => {
                if (process_client_message(ev.xclient)) |nev| return nev;
            },
            c.Expose => {
                if (process_expose(ev.xexpose)) |nev| return nev;
            },
            c.MapNotify => {
                if (process_map_notify(ev.xmap)) |nev| return nev;
            },
            c.ConfigureNotify => {
                if (process_configure_notify(ev.xconfigure)) |nev| return nev;
            },
            c.ReparentNotify => {
                if (process_reparent_notify(ev.xreparent)) |nev| return nev;
            },
            else => {
                std.debug.print("{}\n", .{ev.type});
            },
        }
    }
    return null;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_key_press(ev: c.XKeyEvent) ?ng.Event {
    return .{ .key_down = .{
        .key = ev.keycode,
        .scan_code = 0,
    } };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_key_release(ev: c.XKeyEvent) ?ng.Event {
    return .{ .key_down = .{
        .key = ev.keycode,
        .scan_code = 0,
    } };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_button_press(ev: c.XButtonEvent) ?ng.Event {
    _ = ev;
    return null;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_button_release(ev: c.XButtonEvent) ?ng.Event {
    _ = ev;
    return null;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_motion_notify(ev: c.XMotionEvent) ?ng.Event {
    _ = ev;
    return null;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_enter_window(ev: c.XCrossingEvent) ?ng.Event {
    _ = ev;
    return .{ .enter = .{} };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_leave_window(ev: c.XCrossingEvent) ?ng.Event {
    _ = ev;
    return .{ .leave = .{} };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_focus_in(ev: c.XFocusChangeEvent) ?ng.Event {
    _ = ev;
    return .{ .focus = .{} };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_focus_out(ev: c.XFocusChangeEvent) ?ng.Event {
    _ = ev;
    return .{ .unfocus = .{} };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_map_notify(ev: c.XMapEvent) ?ng.Event {
    _ = ev;
    return null;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_configure_notify(ev: c.XConfigureEvent) ?ng.Event {
    window_width = @floatFromInt(ev.width);
    window_height = @floatFromInt(ev.height);

    return .{
        .resize = .{
            .width = window_width,
            .height = window_height,
        },
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_reparent_notify(ev: c.XReparentEvent) ?ng.Event {
    _ = ev;
    return null;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_client_message(ev: c.XClientMessageEvent) ?ng.Event {
    if (ev.data.l[0] == WM_DELETE_WINDOW) {
        return .{ .quit = .{} };
    }

    return null;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_expose(ev: c.XExposeEvent) ?ng.Event {
    _ = ev;
    return null;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn acquire_command_buffer(self: video.Window) !video.CommandBuffer {
    _ = self;

    return .{
        .handle = 0,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn acquire_swapchain_texture(self: video.CommandBuffer) !video.GPUTexture {
    _ = self;

    return .{
        .handle = 0,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn begin_render_pass(self: video.CommandBuffer, info: video.BeginRenderPassInfo) !video.RenderPass {
    _ = self;

    var width: u32 = @intFromFloat(window_width);
    var height: u32 = @intFromFloat(window_height);
    if (width == 0) width = 1;
    if (height == 0) height = 1;

    api.glViewport(0, 0, width, height);
    api.glScissor(0, 0, width, height);

    // std.debug.print ("{} {}\n", .{width, height});

    if (info.load == .clear) {
        const col = info.clear_color.to_vec4();
        api.glClearColor(col[0], col[1], col[2], col[3]);
        api.glClear(GL_COLOR_BUFFER_BIT);
    }

    return .{
        .handle = 0,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn end_render_pass(self: video.RenderPass) void {
    _ = self;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn submit_command_buffer(self: video.CommandBuffer) !void {
    _ = self;

    api.glXSwapBuffers(display, window);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const GL_Shader = struct {
    program: u32 = 0,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn create_shader(info: video.CreateShaderInfo) video.VideoError!video.Shader {
    const vertex = try compile_shader_part(GL_VERTEX_SHADER, info.vertex_source);
    const fragment = try compile_shader_part(GL_FRAGMENT_SHADER, info.fragment_source);

    defer api.glDeleteShader(vertex);
    defer api.glDeleteShader(fragment);

    const program = try link_shader_parts(vertex, fragment);

    const index = shader_pool.create() orelse return error.TooManyShaders;
    const shader = shader_pool.get(index) orelse return error.TooManyShaders;

    shader.* = .{
        .program = program,
    };

    return .{
        .handle = index,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn compile_shader_part(part: u32, source: [*:0]const u8) video.VideoError!u32 {
    const shader = api.glCreateShader(part);
    if (shader == 0) return error.CannotCreateShader;
    errdefer api.glDeleteShader(shader);

    api.glShaderSource(shader, 1, &source, null);
    api.glCompileShader(shader);

    var status: u32 = undefined;
    api.glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
    if (status == 0) {
        var info_log: [2048]u8 = undefined;
        var length: u32 = undefined;
        api.glGetShaderInfoLog(shader, info_log.len, &length, &info_log);
        std.debug.print("{s}\n", .{info_log[0..length]});
        return error.CannotCreateShader;
    }

    return shader;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn link_shader_parts(vertex: u32, fragment: u32) video.VideoError!u32 {
    const program = api.glCreateProgram();
    errdefer api.glDeleteProgram(program);

    api.glAttachShader(program, vertex);
    api.glAttachShader(program, fragment);
    api.glLinkProgram(program);

    var status: u32 = undefined;
    api.glGetProgramiv(program, GL_LINK_STATUS, &status);
    if (status == 0) {
        var info_log: [2048]u8 = undefined;
        var length: u32 = undefined;
        api.glGetProgramInfoLog(program, info_log.len, &length, &info_log);
        std.debug.print("{s}\n", .{info_log[0..length]});
        return error.CannotCreateShader;
    }

    return program;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn delete_shader(self: video.Shader) void {
    const index = self.handle;
    const shader = shader_pool.get(index) orelse return;

    api.glDeleteProgram(shader.program);

    shader_pool.delete(index);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const GL_Buffer = struct {
    object: u32 = 0,
    kind: video.BufferKind,
    update: video.BufferUpdate,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn create_buffer(info: video.CreateBufferInfo) video.VideoError!video.Buffer {
    const index = buffer_pool.create() orelse return error.TooManyBuffers;
    const buffer = buffer_pool.get(index) orelse return error.TooManyBuffers;

    var buffer_object: u32 = undefined;

    api.glGenBuffers (1, &buffer_object);

    buffer.* = .{
        .object = buffer_object,
        .kind = info.kind,
        .update = info.update,
    };

    if (info.data) |data|
    {
        const gl_update : u32 = switch (info.update)
        {
            .static => GL_STATIC_DRAW,
            .stream => GL_STREAM_DRAW,
            .dynamic => GL_DYNAMIC_DRAW,
        };

        switch (info.kind)
        {
            .vertex_data => {
                api.glBindBuffer (GL_ARRAY_BUFFER, buffer_object);
                api.glBufferData (GL_ARRAY_BUFFER, data.len, data.ptr, gl_update);
                api.glBindBuffer (GL_ARRAY_BUFFER, 0);
            },
            .index_data => {
                api.glBindBuffer (GL_ELEMENT_ARRAY_BUFFER, buffer_object);
                api.glBufferData (GL_ELEMENT_ARRAY_BUFFER, data.len, data.ptr, gl_update);
                api.glBindBuffer (GL_ELEMENT_ARRAY_BUFFER, 0);
            },
        }
    }

    return .{
        .handle = index,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn delete_buffer(self: video.Buffer) void {
    const index = self.handle;
    const buffer = buffer_pool.get(index) orelse return;

    api.glDeleteBuffers(1, &buffer.object);

    shader_pool.delete(index);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
