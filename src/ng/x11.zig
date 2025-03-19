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

const ng = @import("ng");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const video = ng.video;
const event = ng.event;
const debug_text = ng.debug_text;
const Pool = ng.Pool;

const log = ng.Logger(.ng_x11);

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const API = struct {
    XChangeProperty: *const fn (
        *Display,
        Window,
        Atom,
        u32,
        u32,
        u32,
        [*c]const Atom,
        u32,
    ) callconv(.c) void,
    XCloseDisplay: *const fn (*Display) callconv(.c) void,
    XCreateColormap: *const fn (*Display, Window, *c.Visual, u32) callconv(.c) Colormap,
    XCreateIC: *const fn (c.XIM, [*c]const u8, c.XIMStyle, ?*anyopaque) callconv(.c) c.XIC,
    XCreateWindow: *const fn (
        *Display,
        Window,
        u32,
        u32,
        u32,
        u32,
        u32,
        c_int,
        u32,
        *c.Visual,
        CWValueMask,
        *XSetWindowAttributes,
    ) callconv(.c) Window,
    XDefaultScreen: *const fn (*Display) callconv(.c) u32,
    XDefaultScreenOfDisplay: *const fn (*Display) callconv(.c) *Screen,
    XDestroyWindow: *const fn (*Display, Window) callconv(.c) void,
    XDisplayWidth: *const fn (*Display, u32) callconv(.c) i32,
    XDisplayWidthMM: *const fn (*Display, u32) callconv(.c) i32,
    XFilterEvent: *const fn (*const c.XKeyEvent, Window) callconv(.c) bool,
    XFree: *const fn (*anyopaque) callconv(.c) void,
    XGetIMValues: *const fn (c.XIM, ...) callconv(.c) [*c]const u8,
    XInternAtom: *const fn (*Display, [*:0]const u8, bool) callconv(.c) Atom,
    XKeycodeToKeysym: *const fn (*Display, u32, u32) callconv(.c) u32,
    XResizeWindow: *const fn (*Display, Window, i32, i32) callconv(.c) void,
    XMapRaised: *const fn (*Display, Window) callconv(.c) void,
    XNextEvent: *const fn (*Display, *c.XEvent) callconv(.c) void,
    XOpenDisplay: *const fn ([*c]const u8) callconv(.c) *Display,
    XOpenIM: *const fn (*Display, ?*anyopaque, ?*anyopaque, ?*anyopaque) callconv(.c) c.XIM,
    XPending: *const fn (*Display, *c.XEvent) callconv(.c) i32,
    XRootWindowOfScreen: *const fn (*Screen) callconv(.c) Window,
    XSendEvent: *const fn (*Display, Window, bool, u32, [*c]const c.XEvent) callconv(.c) void,
    XSetICValues: *const fn (c.XIC, [*c]const u8, Window, ?*anyopaque) callconv(.c) void,
    XSetLocaleModifiers: *const fn ([*c]const u8) callconv(.c) [*c]const u8,
    XSetWMProtocols: *const fn (*Display, Window, [*c]XID, u32) callconv(.c) void,
    XStoreName: *const fn (*Display, Window, [*:0]const u8) callconv(.c) void,
    XSync: *const fn (*Display, bool) callconv(.c) void,
    XResourceManagerString: ?*const fn (*Display) callconv(.c) [*c]const u8,
    Xutf8LookupString: *const fn (
        c.XIC,
        *const c.XKeyEvent,
        [*c]u8,
        u32,
        ?*anyopaque,
        ?*anyopaque,
    ) callconv(.c) u32,
    glActiveTexture: *const fn (GL_Enum) callconv(.c) void,
    glAttachShader: *const fn (u32, u32) callconv(.c) void,
    glBindBuffer: *const fn (GL_Enum, u32) callconv(.c) void,
    glBindTexture: *const fn (GL_Enum, u32) callconv(.c) void,
    glBindVertexArray: *const fn (u32) callconv(.c) void,
    glBlendFuncSeparate: *const fn (GL_Enum, GL_Enum, GL_Enum, GL_Enum) callconv(.c) void,
    glBufferData: *const fn (GL_Enum, usize, ?*const anyopaque, GL_Enum) callconv(.c) void,
    glBufferSubData: *const fn (GL_Enum, usize, usize, ?*const anyopaque) callconv(.c) void,
    glClear: *const fn (GL_Enum) callconv(.c) void,
    glClearColor: *const fn (f32, f32, f32, f32) callconv(.c) void,
    glCompileShader: *const fn (u32) callconv(.c) void,
    glCreateProgram: *const fn () callconv(.c) u32,
    glCreateShader: *const fn (GL_Enum) callconv(.c) u32,
    glDebugMessageCallback: *const fn (*const fn (
        u32,
        u32,
        u32,
        u32,
        usize,
        [*:0]const u8,
        ?*anyopaque,
    ) callconv(.c) void, ?*anyopaque) callconv(.c) void,
    glDeleteBuffers: *const fn (u32, [*c]u32) callconv(.c) void,
    glDeleteProgram: *const fn (u32) callconv(.c) void,
    glDeleteShader: *const fn (u32) callconv(.c) void,
    glDeleteTextures: *const fn (u32, [*c]u32) callconv(.c) void,
    glDeleteVertexArrays: *const fn (u32, [*c]u32) callconv(.c) void,
    glDisable: *const fn (GL_Enum) callconv(.c) void,
    glDisableVertexAttribArray: *const fn (u32) callconv(.c) void,
    glDrawArrays: *const fn (GL_Primitive, u32, u32) callconv(.c) void,
    glDrawElements: *const fn (GL_Primitive, u32, GL_Enum, usize) callconv(.c) void,
    glEnable: *const fn (GL_Enum) callconv(.c) void,
    glEnableVertexAttribArray: *const fn (u32) callconv(.c) void,
    glFlush: *const fn () callconv(.c) void,
    glGenBuffers: *const fn (u32, [*c]u32) callconv(.c) void,
    glGenTextures: *const fn (u32, [*c]u32) callconv(.c) void,
    glGenVertexArrays: *const fn (u32, [*c]u32) callconv(.c) void,
    glGetProgramInfoLog: *const fn (u32, u32, *u32, [*c]u8) callconv(.c) void,
    glGetProgramiv: *const fn (u32, GL_Enum, *u32) callconv(.c) void,
    glGetShaderInfoLog: *const fn (u32, u32, *u32, [*c]u8) callconv(.c) void,
    glGetShaderiv: *const fn (u32, GL_Enum, *u32) callconv(.c) void,
    glGetString: *const fn (GL_Enum) callconv(.c) [*:0]const u8,
    glGetUniformLocation: *const fn (u32, [*:0]const u8) callconv(.c) u32,
    glLinkProgram: *const fn (u32) callconv(.c) void,
    glObjectLabel: *const fn (GL_Enum, u32, u32, [*c]const u8) callconv(.c) void,
    glScissor: *const fn (i32, i32, u32, u32) callconv(.c) void,
    glShaderSource: *const fn (u32, u32, *const [*:0]const u8, ?*u32) callconv(.c) void,
    glTexImage2D: *const fn (
        GL_Enum,
        u32,
        GL_Enum,
        usize,
        usize,
        u32,
        GL_Enum,
        GL_Enum,
        ?*const anyopaque,
    ) callconv(.c) void,
    glTexParameteri: *const fn (GL_Enum, GL_Enum, GL_Enum) callconv(.c) void,
    glUniform1i: *const fn (u32, u32) callconv(.c) void,
    glUniformMatrix4fv: *const fn (u32, u32, u32, [*c]const u8) callconv(.c) void,
    glUseProgram: *const fn (u32) callconv(.c) void,
    glVertexAttribPointer: *const fn (u32, u32, GL_Enum, bool, usize, usize) callconv(.c) void,
    glViewport: *const fn (u32, u32, u32, u32) callconv(.c) void,
    glXChooseFBConfig: *const fn (
        *Display,
        u32,
        [*c]const u32,
        *u32,
    ) callconv(.c) [*c]c.GLXFBConfig,
    glXCreateContextAttribsARB: ?*const fn (
        *Display,
        c.GLXFBConfig,
        u32,
        u32,
        [*c]const u32,
    ) callconv(.c) ?*GLXContext,
    glXGetFBConfigAttrib: *const fn (*Display, c.GLXFBConfig, u32, *u32) callconv(.c) void,
    glXGetProcAddressARB: *const fn ([*:0]const u8) callconv(.c) ?*anyopaque,
    glXGetVisualFromFBConfig: *const fn (*Display, c.GLXFBConfig) callconv(.c) ?*c.XVisualInfo,
    glXMakeCurrent: *const fn (*Display, Window, *GLXContext) callconv(.c) void,
    glXQueryVersion: *const fn (*Display, *u32, *u32) callconv(.c) bool,
    glXSwapBuffers: *const fn (*Display, Window) callconv(.c) void,
    glXSwapIntervalEXT: ?*const fn (*Display, Window, i32) callconv(.c) void,
    XcursorLibraryLoadCursor: *const fn (*Display, [*c]const u8) callconv(.c) Cursor,
    XDefineCursor: *const fn (*Display, Window, Cursor) callconv(.c) void,
};

var api: API = undefined;

const debug_api = false;

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
var xim: c.XIM = undefined;
var xic: c.XIC = undefined;

var high_dpi_scale: f32 = 2;

var window_width: f32 = 0;
var window_height: f32 = 0;
var window_fullscreen: bool = false;

var current_cursor: video.Cursor = .default;

var shader_pool: Pool(GL_Shader, 256) = .{};
var buffer_pool: Pool(GL_Buffer, 256) = .{};
var pipeline_pool: Pool(GL_Pipeline, 256) = .{};
var bindings_pool: Pool(GL_Bindings, 256) = .{};
var image_pool: Pool(GL_Image, 256) = .{};
var sampler_pool: Pool(GL_Sampler, 256) = .{};

var vao: u32 = 0;
var enabled_attributes: [video.max_vertex_attributes]bool =
    .{false} ** video.max_vertex_attributes;
var draw_primitive: GL_Primitive = .GL_TRIANGLES;
var current_index_type: video.IndexType = .none;
var current_shader: video.Shader = undefined;
var use_glflush: bool = false;

var mouse_buttons: ng.event.MouseButtons = .{};
var mouse_last_click: ng.event.MouseButton = .none;
var mouse_last_click_time: u64 = 0;
var mouse_last_click_position: ng.Vec2 = .{ 0, 0 };
var mouse_last_position: ng.Vec2 = .{ 0, 0 };

var keysyms: [2][256]u32 = undefined;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const XA_ATOM: Atom = 4;
const PropModeReplace = 0;
const PropModePrepend = 1;
const PropModeAppend = 2;
const SubstructureNotifyMask = 1 << 19;
const SubstructureRedirectMask = 1 << 20;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

var WM_DELETE_WINDOW: Atom = undefined;
var _NET_WM_STATE: Atom = undefined;
var _NET_WM_STATE_FULLSCREEN: Atom = undefined;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn init() !video.Platform {
    api = try ng.lookup_symbols(API, &.{
        "libX11.so",
        "libxcb.so",
        "libGLX.so",
        "libGL.so",
        "libXcursor.so",
    });

    display = api.XOpenDisplay(null);

    screen = api.XDefaultScreenOfDisplay(display);
    screen_id = api.XDefaultScreen(display);

    if (api.XResourceManagerString) |func| {
        const cdata = func(display);
        const data = std.mem.span(cdata);
        var lines = std.mem.splitScalar(u8, data, '\n');
        while (lines.next()) |line| {
            if (std.mem.startsWith(u8, line, "Xft.dpi:")) {
                var index: usize = 0;
                while (index < line.len) {
                    if (line[index] == ' ' or line[index] == '\t') break;
                    index += 1;
                }
                while (index < line.len) {
                    if (line[index] != ' ' and line[index] != '\t') break;
                    index += 1;
                }
                const dpi = try std.fmt.parseInt(u8, line[index..], 10);
                high_dpi_scale = std.math.clamp(@as(f32, @floatFromInt(dpi)) / 96, 1, 4);
            }
        }
    }

    log.note("high_dpi_scale = {d}", .{high_dpi_scale});

    root_window = api.XRootWindowOfScreen(screen);

    WM_DELETE_WINDOW = api.XInternAtom(display, "WM_DELETE_WINDOW", false);
    _NET_WM_STATE = api.XInternAtom(display, "_NET_WM_STATE", false);
    _NET_WM_STATE_FULLSCREEN = api.XInternAtom(display, "_NET_WM_STATE_FULLSCREEN", false);

    var glx_major: u32 = 0;
    var glx_minor: u32 = 0;
    if (api.glXQueryVersion(display, &glx_major, &glx_minor)) {
        if (glx_major == 1 and glx_minor < 3) {
            return error.GLXVersionInvalid;
        }
        log.note("GLX v{}.{}", .{ glx_major, glx_minor });
    }

    ng.lookup_using(API, &api, api.glXGetProcAddressARB);

    if (api.glXCreateContextAttribsARB == null) {
        return error.MissingSymbol_glxXCreateContextAttribsARB;
    }

    return .{
        .create_window = create_window,
        .close_window = close_window,
        .get_window_size = get_window_size,
        .set_window_size = set_window_size,
        .set_swap_interval = set_swap_interval,
        .acquire_command_buffer = acquire_command_buffer,
        .toggle_fullscreen = toggle_fullscreen,
        .use_cursor = use_cursor,
        .acknowledge_resize = acknowledge_resize,
        .acquire_swapchain_texture = acquire_swapchain_texture,
        .begin_render_pass = begin_render_pass,
        .end_render_pass = end_render_pass,
        .apply_pipeline = apply_pipeline,
        .apply_bindings = apply_bindings,
        .apply_uniform = apply_uniform,
        .apply_scissor = apply_scissor,
        .get_render_pass_size = get_render_pass_size,
        .draw = draw,
        .submit_command_buffer = submit_command_buffer,
        .deinit = deinit,
        .generate_events = generate_events,
        .create_shader = create_shader,
        .delete_shader = delete_shader,
        .create_buffer = create_buffer,
        .update_buffer = update_buffer,
        .delete_buffer = delete_buffer,
        .create_pipeline = create_pipeline,
        .delete_pipeline = delete_pipeline,
        .create_binding = create_binding,
        .delete_binding = delete_binding,
        .create_image = create_image,
        .delete_image = delete_image,
        .create_sampler = create_sampler,
        .delete_sampler = delete_sampler,
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

fn opengl_debug_message(
    source: u32,
    kind: u32,
    id: u32,
    severity: u32,
    length: usize,
    message: [*:0]const u8,
    _: ?*anyopaque,
) callconv(.c) void {
    if (id == 0x20071) return; // static draw will use video memory

    const source_name = switch (source) {
        0x8246 => "API",
        0x8247 => "Window System",
        0x8248 => "Shader Compiler",
        0x8249 => "Third Party",
        0x824A => "Application",
        0x824B => "Other",
        else => "Unknown",
    };
    const kind_name = switch (kind) {
        0x824C => "Error",
        0x824D => "Deprecated Behavior",
        0x824E => "Undefined Behavior",
        0x824F => "Portability",
        0x8250 => "Performance",
        0x8251 => "Other",
        0x8268 => "Marker",
        0x8269 => "Push Group",
        0x826A => "Pop Group",
        else => "Unknown",
    };
    const severity_name = switch (severity) {
        0x9146 => "High",
        0x9147 => "Medium",
        0x9148 => "Low",
        0x826B => "Notification",
        else => "Unknown",
    };
    switch (severity) {
        0x9146 => {
            log.err("OpenGL: {s} {s} {x} {s} : {s}", .{
                source_name,
                kind_name,
                id,
                severity_name,
                message[0..length],
            });
        },
        0x9147 => {
            log.warn("OpenGL: {s} {s} {x} {s} : {s}", .{
                source_name,
                kind_name,
                id,
                severity_name,
                message[0..length],
            });
        },
        0x9148 => {
            log.info("OpenGL: {s} {s} {x} {s} : {s}", .{
                source_name,
                kind_name,
                id,
                severity_name,
                message[0..length],
            });
        },
        0x826B => {
            log.note("OpenGL: {s} {s} {x} {s} : {s}", .{
                source_name,
                kind_name,
                id,
                severity_name,
                message[0..length],
            });
        },
        else => {
            log.debug("OpenGL: {s} {s} {x} {s} : {s}", .{
                source_name,
                kind_name,
                id,
                severity_name,
                message[0..length],
            });
        },
    }
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
        if (debug_api) {
            log.err("glXChooseFBConfig failed", .{});
        }
        return error.CannotOpenWindow;
    };
    defer api.XFree(@ptrCast(fbc));

    const fb_config = fbc[0];

    const vi = api.glXGetVisualFromFBConfig(display, fb_config) orelse {
        if (debug_api) {
            log.err("glXGetVisualFromFBConfig failed", .{});
        }
        return error.CannotOpenWindow;
    };
    defer api.XFree(vi);

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
        GLX_CONTEXT_FLAGS_ARB,         GLX_CONTEXT_DEBUG_BIT_ARB,
        0,
    };

    glx_ctx = api.glXCreateContextAttribsARB.?(
        display,
        fb_config,
        0,
        c.True,
        &context_attribs,
    ) orelse {
        log.err("glXCreateContextAttribsARB failed", .{});
        return error.CannotOpenWindow;
    };

    api.XSync(display, false);

    api.glXMakeCurrent(display, window, glx_ctx);

    log.note("OpenGL vendor: {s}", .{api.glGetString(.GL_VENDOR)});
    log.note("OpenGL render: {s}", .{api.glGetString(.GL_RENDERER)});
    log.note("OpenGL version: {s}", .{api.glGetString(.GL_VERSION)});

    api.glEnable(.GL_DEBUG_OUTPUT);
    api.glEnable(.GL_DEBUG_OUTPUT_SYNCHRONOUS);

    api.glDebugMessageCallback(opengl_debug_message, null);

    api.XSetWMProtocols(display, window, &WM_DELETE_WINDOW, 1);

    api.XMapRaised(display, window);

    api.glGenVertexArrays(1, &vao);
    if (debug_api) {
        log.debug("glGenVertexArrays {} {}", .{ 1, vao });
    }

    if (debug_api) {
        log.debug("glBindVertexArray {}", .{vao});
    }
    api.glBindVertexArray(vao);

    if (debug_api) {
        log.debug("glObjectLabel {} {} {s}", .{ .GL_VERTEX_ARRAY, vao, "VAO" });
    }
    api.glObjectLabel(.GL_VERTEX_ARRAY, vao, 3, "VAO");

    if (options.name) |name| {
        api.XStoreName(display, window, name);
    }

    for (0..256) |i| {
        const sym0 = api.XKeycodeToKeysym(display, @intCast(i), 0);
        const sym1 = api.XKeycodeToKeysym(display, @intCast(i), 1);
        keysyms[0][i] = sym0;
        keysyms[1][i] = sym1;
    }

    try debug_text.init();

    try init_keyboard();

    return .{
        .handle = window,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn close_window(a_window: video.Window) void {
    debug_text.deinit();
    api.XDestroyWindow(display, a_window.handle);
}
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn get_window_size(_: video.Window) ng.Vec2 {
    return .{
        window_width / high_dpi_scale,
        window_height / high_dpi_scale,
    };
}

fn set_window_size(a_window: video.Window, size: ng.Vec2) void {
    api.XResizeWindow(
        display,
        a_window.handle,
        @intFromFloat(size[0] * high_dpi_scale),
        @intFromFloat(size[1] * high_dpi_scale),
    );
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn set_swap_interval(a_window: video.Window, interval: video.SwapInterval) void {
    if (api.glXSwapIntervalEXT) |swap_interval| {
        const ext_interval: i32 = switch (interval) {
            .fast => 0,
            .vsync, .lowpower => 1,
            .double => 2,
            .adaptive => -1,
        };
        swap_interval(display, a_window.handle, ext_interval);
    }

    if (interval == .lowpower or interval == .double) {
        use_glflush = true;
    } else {
        use_glflush = false;
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const WM_StateAction = enum(u32) {
    _NET_WM_STATE_REMOVE = 0,
    _NET_WM_STATE_ADD = 1,
    _NET_WM_STATE_TOGGLE = 2,
};

fn toggle_fullscreen(_: video.Window) void {
    var action: WM_StateAction = undefined;

    if (window_fullscreen) {
        action = ._NET_WM_STATE_TOGGLE;
        window_fullscreen = false;
    } else {
        action = ._NET_WM_STATE_TOGGLE;
        window_fullscreen = true;
    }

    api.XSync(display, false);

    var e: c.XEvent = undefined;

    e.xany.type = c.ClientMessage;
    e.xclient.serial = 0;
    e.xclient.send_event = c.True;
    e.xclient.message_type = _NET_WM_STATE;
    e.xclient.format = 32;
    e.xclient.window = window;
    e.xclient.data.l[0] = @intFromEnum(action);
    e.xclient.data.l[1] = @intCast(_NET_WM_STATE_FULLSCREEN);
    e.xclient.data.l[2] = 0;
    e.xclient.data.l[3] = 1;
    e.xclient.data.l[4] = 0;

    api.XSendEvent(
        display,
        root_window,
        false,
        SubstructureNotifyMask | SubstructureRedirectMask,
        &e,
    );

    api.XSync(display, false);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn use_cursor(cursor: video.Cursor) void {
    if (cursor != current_cursor) {
        current_cursor = cursor;
        const label = switch (current_cursor) {
            .resize => "nwse-resize",
            .resize_ns => "ns-resize",
            .resize_ew => "ew-resize",
            .move => "fleur",
            else => "default",
        };
        const x11_cursor = api.XcursorLibraryLoadCursor(display, label);
        api.XDefineCursor(display, window, x11_cursor);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn acknowledge_resize(_: video.Window) void {
    api.XSync(display, false);
    api.glFlush();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn init_keyboard() !void {
    var buffer: [256:0]u8 = undefined;
    const old_locale = std.c.setlocale(std.c.LC.CTYPE, null);

    if (old_locale) |loc| {
        for (0..buffer.len, loc) |i, ch| {
            buffer[i] = ch;
            if (ch == 0x00) {
                break;
            }
        }
        buffer[255] = 0x01;
    }

    if (std.c.setlocale(std.c.LC.CTYPE, "") == null) {
        log.warn("Cannot set locale", .{});
    }

    if (api.XSetLocaleModifiers("@im=none") == null) {
        log.warn("Cannot set local modifiers", .{});
    }

    xim = api.XOpenIM(display, null, null, null);

    if (old_locale) |_| {
        _ = std.c.setlocale(std.c.LC.CTYPE, &buffer);
    }

    var xim_style: c.XIMStyle = 0;

    if (xim) |im| {
        var xim_styles: ?*c.XIMStyles = undefined;
        if (api.XGetIMValues(im, c.XNQueryInputStyle, &xim_styles, @as(
            [*c]const u8,
            null,
        )) != null or xim_styles == null) {
            log.warn("Cannot query input style", .{});
        }

        if (xim_styles) |styles| {
            for (0..@intCast(styles.count_styles)) |i| {
                if (styles.supported_styles[i] == c.XIMPreeditNothing | c.XIMStatusNothing) {
                    xim_style = styles.supported_styles[i];
                }
            }
            defer api.XFree(styles);
        }

        if (xim_style == 0) {
            log.warn("Cannot find supported style", .{});
        } else {
            xic = api.XCreateIC(xim, c.XNInputStyle, xim_style, null);
        }
    }

    if (xic) |ic| {
        api.XSetICValues(ic, c.XNClientWindow, window, null);
    }
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

const GL_Primitive = enum(u32) {
    GL_POINTS = 0x0000,
    GL_LINES = 0x0001,
    GL_LINE_STRIP = 0x0003,
    GL_TRIANGLES = 0x0004,
    GL_TRIANGLE_STRIP = 0x0005,
};

const GL_Enum = enum(u32) {
    GL_ZERO = 0,
    GL_ONE = 1,

    GL_COLOR_BUFFER_BIT = 0x4000,

    GL_DEBUG_OUTPUT = 0x92e0,
    GL_DEBUG_OUTPUT_SYNCHRONOUS = 0x8242,

    GL_FRAGMENT_SHADER = 0x8b30,
    GL_VERTEX_SHADER = 0x8b31,
    GL_COMPILE_STATUS = 0x8b81,
    GL_LINK_STATUS = 0x8b82,

    GL_ARRAY_BUFFER = 0x8892,
    GL_ELEMENT_ARRAY_BUFFER = 0x8893,

    GL_STREAM_DRAW = 0x88e0,
    GL_STREAM_READ = 0x88e1,
    GL_STREAM_COPY = 0x88e2,
    GL_STATIC_DRAW = 0x88e4,
    GL_STATIC_READ = 0x88e5,
    GL_STATIC_COPY = 0x88e6,
    GL_DYNAMIC_DRAW = 0x88e8,
    GL_DYNAMIC_READ = 0x88e9,
    GL_DYNAMIC_COPY = 0x88ea,

    GL_BYTE = 0x1400,
    GL_UNSIGNED_BYTE = 0x1401,
    GL_SHORT = 0x1402,
    GL_UNSIGNED_SHORT = 0x1403,
    GL_INT = 0x1404,
    GL_UNSIGNED_INT = 0x1405,
    GL_FLOAT = 0x1406,
    GL_DOUBLE = 0x140A,
    GL_HALF_FLOAT = 0x140B,

    GL_BUFFER = 0x82E0,
    GL_PROGRAM = 0x82E2,
    GL_TEXTURE = 0x1702,
    GL_VERTEX_ARRAY = 0x8074,

    GL_TEXTURE0 = 0x84C0,
    GL_TEXTURE1 = 0x84C1,
    GL_TEXTURE2 = 0x84C2,
    GL_TEXTURE3 = 0x84C3,
    GL_TEXTURE4 = 0x84C4,
    GL_TEXTURE5 = 0x84C5,
    GL_TEXTURE6 = 0x84C6,
    GL_TEXTURE7 = 0x84C7,

    GL_TEXTURE_2D = 0x0DE1,

    GL_TEXTURE_MAG_FILTER = 0x2800,
    GL_TEXTURE_MIN_FILTER = 0x2801,
    GL_TEXTURE_WRAP_S = 0x2802,
    GL_TEXTURE_WRAP_T = 0x2803,

    GL_NEAREST = 0x2600,
    GL_LINEAR = 0x2601,

    GL_REPEAT = 0x2901,
    GL_CLAMP_TO_EDGE = 0x812F,
    GL_MIRRORED_REPEAT = 0x8370,

    GL_BLEND = 0x0BE2,
    GL_SCISSOR_TEXT = 0x0C11,

    GL_ONE_MINUS_SRC_ALPHA = 0x0303,
    GL_SRC_ALPHA = 0x0302,

    GL_R8 = 0x8229,
    GL_RED = 0x1903,

    GL_VENDOR = 0x1F00,
    GL_RENDERER = 0x1F01,
    GL_VERSION = 0x1F02,
};

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

fn generate_events() void {
    var ev: c.XEvent = undefined;

    while (api.XPending(display, &ev) > 0) {
        api.XNextEvent(display, &ev);

        switch (ev.type) {
            c.KeyPress => {
                process_key_press(&ev.xkey);
            },
            c.KeyRelease => {
                process_key_release(ev.xkey);
            },
            c.ButtonPress => {
                process_button_press(ev.xbutton);
            },
            c.ButtonRelease => {
                process_button_release(ev.xbutton);
            },
            c.EnterNotify => {
                process_enter_window(ev.xcrossing);
            },
            c.LeaveNotify => {
                process_leave_window(ev.xcrossing);
            },
            c.FocusIn => {
                process_focus_in(ev.xfocus);
            },
            c.FocusOut => {
                process_focus_out(ev.xfocus);
            },
            c.MotionNotify => {
                process_motion_notify(ev.xmotion);
            },
            c.ClientMessage => {
                process_client_message(ev.xclient);
            },
            c.Expose => {
                process_expose(ev.xexpose);
            },
            c.MapNotify => {
                process_map_notify(ev.xmap);
            },
            c.ConfigureNotify => {
                process_configure_notify(ev.xconfigure);
            },
            c.ReparentNotify => {
                process_reparent_notify(ev.xreparent);
            },
            else => {
                log.debug("Unknown x11 event type: {}", .{ev.type});
            },
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_key_press(ev: *const c.XKeyEvent) void {
    if (ev.keycode > 0) {
        const key = get_key(ev.keycode);

        ng.key_pressed[@intFromEnum(key)] = true;
        ng.key_down[@intFromEnum(key)] = true;

        ng.send_event(.{ .key_down = .{
            .scan_code = ev.keycode,
            .key = key,
        } });
    }

    var utf8_buffer: [16]u8 = undefined;
    var utf8_len: u32 = 0;

    if (xic) |ic| {
        utf8_len = api.Xutf8LookupString(ic, ev, &utf8_buffer, utf8_buffer.len, null, null);
    }

    if (api.XFilterEvent(ev, window)) {
        return;
    }

    if (utf8_len == 0) {
        return;
    }

    const view = std.unicode.Utf8View.init(utf8_buffer[0..utf8_len]) catch {
        return;
    };

    var iter = view.iterator();
    while (iter.nextCodepoint()) |cp| {
        ng.send_event(.{ .text = .{
            .cp = cp,
        } });
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_key_release(ev: c.XKeyEvent) void {
    const key = get_key(ev.keycode);

    ng.key_down[@intFromEnum(key)] = false;

    ng.send_event(.{ .key_up = .{
        .scan_code = ev.keycode,
        .key = key,
    } });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const KeySymTable = struct { sym: u32, key: event.Key };

const keysym_table = [_]KeySymTable{
    .{ .sym = 0x20, .key = .space },
    .{ .sym = 0x21, .key = .@"!" },
    .{ .sym = 0x22, .key = .@"\"" },
    .{ .sym = 0x23, .key = .@"#" },
    .{ .sym = 0x24, .key = .@"$" },
    .{ .sym = 0x25, .key = .@"%" },
    .{ .sym = 0x26, .key = .@"&" },
    .{ .sym = 0x27, .key = .@"'" },
    .{ .sym = 0x28, .key = .@"(" },
    .{ .sym = 0x29, .key = .@")" },
    .{ .sym = 0x2a, .key = .@"*" },
    .{ .sym = 0x2b, .key = .@"+" },
    .{ .sym = 0x2c, .key = .@"," },
    .{ .sym = 0x2d, .key = .@"-" },
    .{ .sym = 0x2e, .key = .@"." },
    .{ .sym = 0x2f, .key = .@"/" },
    .{ .sym = 0x30, .key = .@"0" },
    .{ .sym = 0x31, .key = .@"1" },
    .{ .sym = 0x32, .key = .@"2" },
    .{ .sym = 0x33, .key = .@"3" },
    .{ .sym = 0x34, .key = .@"4" },
    .{ .sym = 0x35, .key = .@"5" },
    .{ .sym = 0x36, .key = .@"6" },
    .{ .sym = 0x37, .key = .@"7" },
    .{ .sym = 0x38, .key = .@"8" },
    .{ .sym = 0x39, .key = .@"9" },
    .{ .sym = 0x3a, .key = .@":" },
    .{ .sym = 0x3b, .key = .@";" },
    .{ .sym = 0x3c, .key = .@"<" },
    .{ .sym = 0x3d, .key = .@"=" },
    .{ .sym = 0x3e, .key = .@">" },
    .{ .sym = 0x3f, .key = .@"?" },
    .{ .sym = 0x40, .key = .@"@" },
    .{ .sym = 0x41, .key = .A },
    .{ .sym = 0x42, .key = .B },
    .{ .sym = 0x43, .key = .C },
    .{ .sym = 0x44, .key = .D },
    .{ .sym = 0x45, .key = .E },
    .{ .sym = 0x46, .key = .F },
    .{ .sym = 0x47, .key = .G },
    .{ .sym = 0x48, .key = .H },
    .{ .sym = 0x49, .key = .I },
    .{ .sym = 0x4a, .key = .J },
    .{ .sym = 0x4b, .key = .K },
    .{ .sym = 0x4c, .key = .L },
    .{ .sym = 0x4d, .key = .M },
    .{ .sym = 0x4e, .key = .N },
    .{ .sym = 0x4f, .key = .O },
    .{ .sym = 0x50, .key = .P },
    .{ .sym = 0x51, .key = .Q },
    .{ .sym = 0x52, .key = .R },
    .{ .sym = 0x53, .key = .S },
    .{ .sym = 0x54, .key = .T },
    .{ .sym = 0x55, .key = .U },
    .{ .sym = 0x56, .key = .V },
    .{ .sym = 0x57, .key = .W },
    .{ .sym = 0x58, .key = .X },
    .{ .sym = 0x59, .key = .Y },
    .{ .sym = 0x5a, .key = .Z },
    .{ .sym = 0x5b, .key = .@"[" },
    .{ .sym = 0x5c, .key = .@"\\" },
    .{ .sym = 0x5d, .key = .@"]" },
    .{ .sym = 0x5e, .key = .@"^" },
    .{ .sym = 0x5f, .key = ._ },
    .{ .sym = 0x60, .key = .@"`" },
    .{ .sym = 0x61, .key = .A },
    .{ .sym = 0x62, .key = .B },
    .{ .sym = 0x63, .key = .C },
    .{ .sym = 0x64, .key = .D },
    .{ .sym = 0x65, .key = .E },
    .{ .sym = 0x66, .key = .F },
    .{ .sym = 0x67, .key = .G },
    .{ .sym = 0x68, .key = .H },
    .{ .sym = 0x69, .key = .I },
    .{ .sym = 0x6a, .key = .J },
    .{ .sym = 0x6b, .key = .K },
    .{ .sym = 0x6c, .key = .L },
    .{ .sym = 0x6d, .key = .M },
    .{ .sym = 0x6e, .key = .N },
    .{ .sym = 0x6f, .key = .O },
    .{ .sym = 0x70, .key = .P },
    .{ .sym = 0x71, .key = .Q },
    .{ .sym = 0x72, .key = .R },
    .{ .sym = 0x73, .key = .S },
    .{ .sym = 0x74, .key = .T },
    .{ .sym = 0x75, .key = .U },
    .{ .sym = 0x76, .key = .V },
    .{ .sym = 0x77, .key = .W },
    .{ .sym = 0x78, .key = .X },
    .{ .sym = 0x79, .key = .Y },
    .{ .sym = 0x7a, .key = .Z },
    .{ .sym = 0x7b, .key = .@"{" },
    .{ .sym = 0x7c, .key = .@"|" },
    .{ .sym = 0x7d, .key = .@"}" },
    .{ .sym = 0x7e, .key = .@"~" },
    .{ .sym = 0xff08, .key = .backspace },
    .{ .sym = 0xff09, .key = .tab },
    .{ .sym = 0xff0d, .key = .enter },
    .{ .sym = 0xff13, .key = .pause },
    .{ .sym = 0xff14, .key = .scroll_lock },
    .{ .sym = 0xff1b, .key = .escape },
    .{ .sym = 0xff20, .key = .compose },
    .{ .sym = 0xff50, .key = .home },
    .{ .sym = 0xff51, .key = .left },
    .{ .sym = 0xff52, .key = .up },
    .{ .sym = 0xff53, .key = .right },
    .{ .sym = 0xff54, .key = .down },
    .{ .sym = 0xff55, .key = .pageup },
    .{ .sym = 0xff56, .key = .pagedown },
    .{ .sym = 0xff57, .key = .end },
    .{ .sym = 0xff63, .key = .insert },
    .{ .sym = 0xff67, .key = .menu },
    .{ .sym = 0xffbe, .key = .f1 },
    .{ .sym = 0xffbf, .key = .f2 },
    .{ .sym = 0xffc0, .key = .f3 },
    .{ .sym = 0xffc1, .key = .f4 },
    .{ .sym = 0xffc2, .key = .f5 },
    .{ .sym = 0xffc3, .key = .f6 },
    .{ .sym = 0xffc4, .key = .f7 },
    .{ .sym = 0xffc5, .key = .f8 },
    .{ .sym = 0xffc6, .key = .f9 },
    .{ .sym = 0xffc7, .key = .f10 },
    .{ .sym = 0xffc8, .key = .f11 },
    .{ .sym = 0xffc9, .key = .f12 },
    .{ .sym = 0xffe1, .key = .leftshift },
    .{ .sym = 0xffe2, .key = .rightshift },
    .{ .sym = 0xffe3, .key = .leftctrl },
    .{ .sym = 0xffe4, .key = .rightctrl },
    .{ .sym = 0xffe5, .key = .capslock },
    .{ .sym = 0xffe6, .key = .shiftlock },
    .{ .sym = 0xffe7, .key = .leftmeta },
    .{ .sym = 0xffe8, .key = .rightmeta },
    .{ .sym = 0xffe9, .key = .leftalt },
    .{ .sym = 0xffea, .key = .rightalt },
    .{ .sym = 0xffeb, .key = .leftsuper },
    .{ .sym = 0xffec, .key = .rightsuper },
    .{ .sym = 0xffff, .key = .delete },
};

fn get_key(code: u32) event.Key {
    const sym = keysyms[0][code];
    for (keysym_table) |entry| {
        if (entry.sym == sym) return entry.key;
    }
    log.debug("Unknown {x} {}", .{ sym, code });
    return .unknown;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_button_press(ev: c.XButtonEvent) void {
    switch (ev.button) {
        1 => {
            mouse_buttons.left = true;
            const now = ng.time.elapsed_us();
            if (mouse_last_click == .left) {
                const elapsed = now - mouse_last_click_time;
                if (elapsed < 500_000) // 500 ms double click time
                {
                    ng.send_event(.{ .mouse_double_click = .{
                        .button = .left,
                        .pos = mouse_last_position,
                    } });
                    mouse_last_click = .none;
                    mouse_last_click_time = 0;
                    return;
                }
            }
            ng.send_event(.{ .mouse_down = .{
                .button = .left,
                .pos = mouse_last_position,
            } });
            mouse_last_click = .left;
            mouse_last_click_time = now;
            mouse_last_click_position = mouse_last_position;
        },
        2 => {
            mouse_buttons.middle = true;
            ng.send_event(.{ .mouse_down = .{
                .button = .middle,
                .pos = mouse_last_position,
            } });
            mouse_last_click = .none;
            mouse_last_click_time = 0;
        },
        3 => {
            mouse_buttons.right = true;
            ng.send_event(.{ .mouse_down = .{
                .button = .right,
                .pos = mouse_last_position,
            } });
            mouse_last_click = .none;
            mouse_last_click_time = 0;
        },
        4 => {
            ng.send_event(.{ .mouse_wheel = .{
                .dy = 1,
            } });
        },
        5 => {
            ng.send_event(.{ .mouse_wheel = .{
                .dy = -1,
            } });
        },
        6 => {
            ng.send_event(.{ .mouse_wheel = .{
                .dx = 1,
            } });
        },
        7 => {
            ng.send_event(.{ .mouse_wheel = .{
                .dx = -1,
            } });
        },
        8 => {
            mouse_buttons.x1 = true;
            ng.send_event(.{ .mouse_down = .{
                .button = .x1,
                .pos = mouse_last_position,
            } });
            mouse_last_click = .none;
            mouse_last_click_time = 0;
        },
        9 => {
            mouse_buttons.x2 = true;
            ng.send_event(.{ .mouse_down = .{
                .button = .x2,
                .pos = mouse_last_position,
            } });
            mouse_last_click = .none;
            mouse_last_click_time = 0;
        },
        else => {},
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_button_release(ev: c.XButtonEvent) void {
    switch (ev.button) {
        1 => {
            mouse_buttons.left = false;
            ng.send_event(.{ .mouse_up = .{
                .button = .left,
                .pos = mouse_last_position,
            } });
        },
        2 => {
            mouse_buttons.middle = false;
            ng.send_event(.{ .mouse_up = .{
                .button = .middle,
                .pos = mouse_last_position,
            } });
        },
        3 => {
            mouse_buttons.right = false;
            ng.send_event(.{ .mouse_up = .{
                .button = .right,
                .pos = mouse_last_position,
            } });
        },
        8 => {
            mouse_buttons.x1 = false;
            ng.send_event(.{ .mouse_up = .{
                .button = .x1,
                .pos = mouse_last_position,
            } });
        },
        9 => {
            mouse_buttons.x2 = false;
            ng.send_event(.{ .mouse_up = .{
                .button = .x2,
                .pos = mouse_last_position,
            } });
        },
        else => {},
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_motion_notify(ev: c.XMotionEvent) void {
    const x: f32 = @as(f32, @floatFromInt(ev.x)) / high_dpi_scale;
    const y: f32 = @as(f32, @floatFromInt(ev.y)) / high_dpi_scale;
    ng.send_event(.{ .mouse_move = .{
        .pos = .{ x, y },
        .buttons = mouse_buttons,
    } });

    if (mouse_last_click == .left) {
        if (@abs(mouse_last_click_position[0] - x) > 8 or
            @abs(mouse_last_click_position[1] - y) > 8)
        {
            mouse_last_click = .none;
        }
    }

    mouse_last_position = .{ x, y };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_enter_window(ev: c.XCrossingEvent) void {
    _ = ev;
    ng.send_event(.{ .enter = .{} });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_leave_window(ev: c.XCrossingEvent) void {
    _ = ev;
    ng.send_event(.{ .leave = .{} });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_focus_in(ev: c.XFocusChangeEvent) void {
    _ = ev;
    ng.send_event(.{ .focus = .{} });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_focus_out(ev: c.XFocusChangeEvent) void {
    _ = ev;
    ng.send_event(.{ .unfocus = .{} });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_map_notify(ev: c.XMapEvent) void {
    _ = ev;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_configure_notify(ev: c.XConfigureEvent) void {
    window_width = @floatFromInt(ev.width);
    window_height = @floatFromInt(ev.height);

    ng.send_event(.{
        .resize = .{
            .width = window_width / high_dpi_scale,
            .height = window_height / high_dpi_scale,
        },
    });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_reparent_notify(ev: c.XReparentEvent) void {
    _ = ev;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_client_message(ev: c.XClientMessageEvent) void {
    if (ev.data.l[0] == WM_DELETE_WINDOW) {
        ng.send_event(.{ .quit = .{} });
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_expose(ev: c.XExposeEvent) void {
    _ = ev;
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

fn begin_render_pass(
    self: video.CommandBuffer,
    info: video.BeginRenderPassInfo,
) !video.RenderPass {
    _ = self;

    var width: u32 = @intFromFloat(window_width);
    var height: u32 = @intFromFloat(window_height);
    if (width == 0) width = 1;
    if (height == 0) height = 1;

    if (debug_api) {
        log.debug("glViewport {} {} {} {}", .{ 0, 0, width, height });
    }
    api.glViewport(0, 0, width, height);
    if (debug_api) {
        log.debug("glScissor {} {} {} {}", .{ 0, 0, width, height });
    }
    api.glScissor(0, 0, width, height);

    if (info.load == .clear) {
        const col = info.clear_color.to_vec4();
        if (debug_api) {
            log.debug("glClearColor {} {} {} {}", .{ col[0], col[1], col[2], col[3] });
        }
        api.glClearColor(col[0], col[1], col[2], col[3]);
        if (debug_api) {
            log.debug("glClear {}", .{.GL_COLOR_BUFFER_BIT});
        }
        api.glClear(.GL_COLOR_BUFFER_BIT);
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
    if (debug_api) {
        log.debug("glBindBuffer {} {}", .{ GL_Enum.GL_ARRAY_BUFFER, 0 });
    }
    api.glBindBuffer(.GL_ARRAY_BUFFER, 0);
    if (debug_api) {
        log.debug("glUseProgram {}", .{0});
    }
    api.glUseProgram(0);
    for (0..enabled_attributes.len) |i| {
        if (enabled_attributes[i]) {
            if (debug_api) {
                log.debug("glDisableVertexAttribArray {}", .{i});
            }
            api.glDisableVertexAttribArray(@intCast(i));
            enabled_attributes[i] = false;
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn apply_pipeline(self: video.RenderPass, opaque_pipeline: video.Pipeline) void {
    _ = self;

    const index = opaque_pipeline.handle;
    const pipeline = pipeline_pool.get(index) orelse return;

    const shader = pipeline.shader;
    const gl_shader = shader_pool.get(shader.handle) orelse return;

    current_shader = shader;
    if (debug_api) {
        log.debug("glUseProgram {} {?s}", .{ gl_shader.program, gl_shader.label });
    }
    api.glUseProgram(gl_shader.program);

    draw_primitive = switch (pipeline.primitive) {
        .triangle_list => .GL_TRIANGLES,
        .triangle_strip => .GL_TRIANGLE_STRIP,
        .line_list => .GL_LINES,
        .line_strip => .GL_LINE_STRIP,
        .point_list => .GL_POINTS,
    };

    current_index_type = pipeline.index_type;

    if (pipeline.blend.enabled) {
        api.glEnable(.GL_BLEND);
    } else {
        api.glDisable(.GL_BLEND);
    }

    api.glBlendFuncSeparate(
        gl_blend_factor(pipeline.blend.src_factor_rgb),
        gl_blend_factor(pipeline.blend.dst_factor_rgb),
        gl_blend_factor(pipeline.blend.src_factor_alpha),
        gl_blend_factor(pipeline.blend.dst_factor_alpha),
    );

    if (pipeline.scissor_test) {
        api.glEnable(.GL_SCISSOR_TEXT);
    } else {
        api.glDisable(.GL_SCISSOR_TEXT);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn gl_blend_factor(factor: video.BlendFactor) GL_Enum {
    return switch (factor) {
        .one => .GL_ONE,
        .zero => .GL_ZERO,
        .one_minus_src_alpha => .GL_ONE_MINUS_SRC_ALPHA,
        .src_alpha => .GL_SRC_ALPHA,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn apply_bindings(self: video.RenderPass, opaque_binding: video.Binding) void {
    _ = self;

    const index = opaque_binding.handle;
    const binding = bindings_pool.get(index) orelse return;

    for (0.., binding.vertex_buffers) |i, optional_buffer| {
        _ = i;
        if (optional_buffer) |buf| {
            if (buffer_pool.get(buf.handle)) |buffer| {
                if (debug_api) {
                    log.debug("glBindBuffer {} {}", .{
                        GL_Enum.GL_ARRAY_BUFFER,
                        buffer.object,
                    });
                }
                api.glBindBuffer(.GL_ARRAY_BUFFER, buffer.object);
            }
        }
    }

    for (0.., binding.index_buffers) |i, optional_buffer| {
        _ = i;
        if (optional_buffer) |buf| {
            if (buffer_pool.get(buf.handle)) |buffer| {
                if (debug_api) {
                    log.debug("glBindBuffer {} {}", .{
                        GL_Enum.GL_ELEMENT_ARRAY_BUFFER,
                        buffer.object,
                    });
                }
                api.glBindBuffer(.GL_ELEMENT_ARRAY_BUFFER, buffer.object);
            }
        }
    }

    if (binding.image) |image| {
        if (binding.sampler) |sampler| {
            apply_image(image, sampler);
        }
    }

    apply_shader(current_shader);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn apply_uniform(self: video.RenderPass, info: video.UniformInfo) void {
    _ = self;

    const gl_shader = shader_pool.get(current_shader.handle) orelse return;
    const loc = api.glGetUniformLocation(gl_shader.program, info.name);
    if (loc != info.index) {
        log.err("Mismatch on uniform {?s} {s} {} = {}", .{
            gl_shader.label,
            info.name,
            info.index,
            loc,
        });
        return;
    }

    switch (info.kind) {
        .mat4 => {
            if (debug_api) {
                log.debug("glUniformMatrix4fv {} {} {} {*}", .{
                    info.index,
                    1,
                    0,
                    info.data.ptr,
                });
                const value: *const ng.Mat4 = @alignCast(@ptrCast(info.data.ptr));
                log.debug("  {d:0.2}", .{value.*});
            }
            api.glUniformMatrix4fv(info.index, 1, 0, info.data.ptr);
        },
        .u32 => {
            const value: *const u32 = @alignCast(@ptrCast(info.data.ptr));
            if (debug_api) {
                log.debug("glUniform1i {} {}", .{ info.index, value.* });
            }
            api.glUniform1i(info.index, value.*);
        },
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn apply_scissor(self: video.RenderPass, pos: ng.Vec2, size: ng.Vec2) void {
    _ = self;

    const x: i32 = @intFromFloat(pos[0] * high_dpi_scale);
    const y: i32 = @intFromFloat(window_height - (pos[1] + size[1]) * high_dpi_scale);
    const w: u32 = @intFromFloat(size[0] * high_dpi_scale);
    const h: u32 = @intFromFloat(size[1] * high_dpi_scale);

    if (debug_api) {
        log.debug("glScissor {} {} {} {}", .{ x, y, w, h });
    }

    api.glScissor(x, y, w, h);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn get_render_pass_size(self: video.RenderPass) ng.Vec2 {
    _ = self;
    return .{ window_width / high_dpi_scale, window_height / high_dpi_scale };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn draw(self: video.RenderPass, start: usize, count: usize) void {
    _ = self;

    if (debug_api) {
        log.debug("glDrawArrays {} {} {}", .{ draw_primitive, start, count });
    }
    switch (current_index_type) {
        .none => {
            api.glDrawArrays(draw_primitive, @intCast(start), @intCast(count));
        },
        .u16 => {
            const base = start * 2;
            api.glDrawElements(draw_primitive, @intCast(count), .GL_UNSIGNED_SHORT, base);
        },
        .u32 => {
            const base = start * 4;
            api.glDrawElements(draw_primitive, @intCast(count), .GL_UNSIGNED_INT, base);
        },
    }
    if (debug_api) {
        log.debug("     -----", .{});
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn submit_command_buffer(self: video.CommandBuffer) !void {
    _ = self;

    if (debug_api) {
        log.debug("glXSwapBuffers", .{});
        log.debug("glFlush", .{});
        log.debug("----------------------------------------", .{});
    }
    api.glXSwapBuffers(display, window);
    if (use_glflush) {
        api.glFlush();
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const GL_Shader = struct {
    label: ?[]const u8 = null,
    program: u32 = 0,
    vertex_attrib: [video.max_vertex_attributes]video.VertexAttribute = undefined,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn create_shader(info: video.CreateShaderInfo) video.VideoError!video.Shader {
    const vertex = try compile_shader_part(.GL_VERTEX_SHADER, info.vertex_source);
    const fragment = try compile_shader_part(.GL_FRAGMENT_SHADER, info.fragment_source);

    defer api.glDeleteShader(vertex);
    defer api.glDeleteShader(fragment);

    const program = try link_shader_parts(vertex, fragment);

    if (info.label) |label| {
        if (debug_api) {
            log.debug("glObjectLabel {} {} {s}", .{ .GL_PROGRAM, program, label });
        }
        api.glObjectLabel(.GL_PROGRAM, program, @intCast(label.len), label.ptr);
    }

    const index = shader_pool.create() orelse return error.TooManyShaders;
    const shader = shader_pool.get(index) orelse return error.TooManyShaders;

    shader.* = .{
        .label = info.label,
        .program = program,
    };

    for (0.., info.vertex_attrib) |i, attrib| {
        shader.vertex_attrib[i] = attrib;
    }
    return .{
        .handle = index,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn compile_shader_part(part: GL_Enum, source: [*:0]const u8) video.VideoError!u32 {
    const shader = api.glCreateShader(part);
    if (shader == 0) return error.CannotCreateShader;
    errdefer api.glDeleteShader(shader);

    api.glShaderSource(shader, 1, &source, null);
    api.glCompileShader(shader);

    var status: u32 = undefined;
    api.glGetShaderiv(shader, .GL_COMPILE_STATUS, &status);
    if (status == 0) {
        var info_log: [2048]u8 = undefined;
        var length: u32 = undefined;
        api.glGetShaderInfoLog(shader, info_log.len, &length, &info_log);
        log.debug("{s}", .{info_log[0..length]});
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
    api.glGetProgramiv(program, .GL_LINK_STATUS, &status);
    if (status == 0) {
        var info_log: [2048]u8 = undefined;
        var length: u32 = undefined;
        api.glGetProgramInfoLog(program, info_log.len, &length, &info_log);
        log.err("{s}", .{info_log[0..length]});
        return error.CannotCreateShader;
    }

    return program;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn apply_shader(self: video.Shader) void {
    const index = self.handle;
    const shader = shader_pool.get(index) orelse return;

    for (0.., shader.vertex_attrib) |i, attrib| {
        if (attrib.vertex_type != .unknown) {
            const gl_type = get_gl_vertex_type(attrib.vertex_type);
            if (debug_api) {
                log.debug("glEnableVertexAttribArray {}", .{i});
            }
            api.glEnableVertexAttribArray(@intCast(i));
            if (debug_api) {
                log.debug("glVertexAttribPointer {} {} {} {} {} {}", .{
                    i,
                    attrib.size,
                    gl_type,
                    attrib.normalize,
                    attrib.stride,
                    attrib.offset,
                });
            }
            api.glVertexAttribPointer(
                @intCast(i),
                @intCast(attrib.size),
                gl_type,
                attrib.normalize,
                attrib.stride,
                attrib.offset,
            );
            enabled_attributes[i] = true;
        } else if (enabled_attributes[i]) {
            if (debug_api) {
                log.debug("glDisableVertexAttribArray {}", .{i});
            }
            api.glDisableVertexAttribArray(@intCast(i));
            enabled_attributes[i] = false;
        }
    }
}

fn get_gl_vertex_type(vt: video.VertexType) GL_Enum {
    return switch (vt) {
        .unknown => unreachable,
        .u8 => .GL_UNSIGNED_BYTE,
        .i8 => .GL_BYTE,
        .u16 => .GL_UNSIGNED_SHORT,
        .i16 => .GL_SHORT,
        .u32 => .GL_UNSIGNED_INT,
        .i32 => .GL_INT,
        .f32 => .GL_FLOAT,
        .f64 => .GL_DOUBLE,
        .f16 => .GL_HALF_FLOAT,
    };
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
    label: ?[]const u8 = null,
    object: u32 = 0,
    size: ?usize = null,
    kind: video.BufferKind,
    update: video.BufferUpdate,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn create_buffer(info: video.CreateBufferInfo) video.VideoError!video.Buffer {
    if (info.size != null and info.data != null) return error.InvalidCreateInfo;
    if (info.size == null and info.data == null) return error.InvalidCreateInfo;
    const index = buffer_pool.create() orelse return error.TooManyBuffers;
    const buffer = buffer_pool.get(index) orelse return error.TooManyBuffers;

    var buffer_object: u32 = undefined;

    api.glGenBuffers(1, &buffer_object);
    if (debug_api) {
        log.debug("glGenBuffers {} {}", .{ 1, buffer_object });
    }

    buffer.* = .{
        .label = info.label,
        .object = buffer_object,
        .kind = info.kind,
        .size = info.size,
        .update = info.update,
    };

    const gl_update: GL_Enum = switch (info.update) {
        .static => .GL_STATIC_DRAW,
        .stream => .GL_STREAM_DRAW,
        .dynamic => .GL_DYNAMIC_DRAW,
    };

    const kind: GL_Enum = switch (info.kind) {
        .vertex => .GL_ARRAY_BUFFER,
        .index => .GL_ELEMENT_ARRAY_BUFFER,
    };

    if (info.data) |data| {
        if (debug_api) {
            log.debug("glBindBuffer {} {}", .{ kind, buffer_object });
        }
        api.glBindBuffer(kind, buffer_object);
        if (debug_api) {
            log.debug("glBufferData {} {} {*} {}", .{
                kind,
                data.len,
                data.ptr,
                gl_update,
            });
        }
        api.glBufferData(kind, data.len, data.ptr, gl_update);
        if (debug_api) {
            log.debug("glBindBuffer {} {}", .{ kind, 0 });
        }
        api.glBindBuffer(kind, 0);
    } else if (info.size) |size| {
        if (debug_api) {
            log.debug("glBindBuffer {} {}", .{ kind, buffer_object });
        }
        api.glBindBuffer(kind, buffer_object);
        if (debug_api) {
            log.debug("glBufferData {} {} {} {}", .{ kind, size, null, gl_update });
        }
        api.glBufferData(kind, size, null, gl_update);
        if (debug_api) {
            log.debug("glBindBuffer {} {}", .{ kind, 0 });
        }
        api.glBindBuffer(kind, 0);
    }

    if (info.label) |label| {
        if (debug_api) {
            log.debug("glObjectLabel {} {} {s}", .{ .GL_BUFFER, buffer_object, label });
        }
        api.glObjectLabel(.GL_BUFFER, buffer_object, @intCast(label.len), label.ptr);
    }

    return .{
        .handle = index,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn bind_array_buffer(self: video.Buffer) void {
    _ = self;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn update_buffer(self: video.Buffer, data: []const u8) void {
    const index = self.handle;
    const buffer = buffer_pool.get(index) orelse return;

    const kind: GL_Enum = switch (buffer.kind) {
        .vertex => .GL_ARRAY_BUFFER,
        .index => .GL_ELEMENT_ARRAY_BUFFER,
    };

    if (debug_api) {
        log.debug("glBindBuffer {} {}", .{ kind, buffer.object });
    }
    api.glBindBuffer(kind, buffer.object);
    if (debug_api) {
        log.debug("glBufferSubData {} {} {} {*}", .{ kind, 0, data.len, data.ptr });
    }
    api.glBufferSubData(kind, 0, data.len, data.ptr);
    if (debug_api) {
        log.debug("glBindBuffer {} {}", .{ kind, 0 });
    }
    api.glBindBuffer(kind, 0);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn delete_buffer(self: video.Buffer) void {
    const index = self.handle;
    const buffer = buffer_pool.get(index) orelse return;

    api.glDeleteBuffers(1, &buffer.object);

    buffer_pool.delete(index);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const GL_Pipeline = struct {
    label: ?[]const u8 = null,
    shader: video.Shader,
    primitive: video.Primitive,
    index_type: video.IndexType,
    blend: video.BlendInfo,
    scissor_test: bool,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn create_pipeline(info: video.CreatePipelineInfo) video.VideoError!video.Pipeline {
    const index = pipeline_pool.create() orelse return error.TooManyPipelines;
    const pipeline = pipeline_pool.get(index) orelse return error.TooManyPipelines;

    pipeline.* = .{
        .label = info.label,
        .shader = info.shader,
        .primitive = info.primitive,
        .index_type = info.index_type,
        .blend = info.blend,
        .scissor_test = info.scissor_test,
    };

    return .{
        .handle = index,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn delete_pipeline(self: video.Pipeline) void {
    const index = self.handle;
    const pipeline = pipeline_pool.get(index) orelse return;

    _ = pipeline;

    pipeline_pool.delete(index);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const max_vertex_buffers = 8;
const max_index_buffers = 8;

const GL_Bindings = struct {
    label: ?[]const u8 = null,
    vertex_buffers: [max_vertex_buffers]?video.Buffer = .{null} ** max_vertex_buffers,
    index_buffers: [max_index_buffers]?video.Buffer = .{null} ** max_index_buffers,
    image: ?video.Image = null,
    sampler: ?video.Sampler = null,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn create_binding(info: video.CreateBindingInfo) video.VideoError!video.Binding {
    const index = bindings_pool.create() orelse return error.TooManyBindings;
    const binding = bindings_pool.get(index) orelse return error.TooManyBindings;

    binding.* = .{
        .label = info.label,
    };

    if (info.vertex_buffers) |vertex_buffers| {
        for (0.., vertex_buffers) |i, buf| {
            binding.vertex_buffers[i] = buf;
        }
    }

    if (info.index_buffers) |index_buffers| {
        for (0.., index_buffers) |i, buf| {
            binding.index_buffers[i] = buf;
        }
    }

    binding.image = info.image;
    binding.sampler = info.sampler;

    return .{
        .handle = index,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn delete_binding(self: video.Binding) void {
    const index = self.handle;
    const binding = bindings_pool.get(index) orelse return;

    _ = binding;

    bindings_pool.delete(index);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const GL_Image = struct {
    label: ?[]const u8 = null,
    object: u32 = 0,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn create_image(info: video.CreateImageInfo) video.VideoError!video.Image {
    const index = image_pool.create() orelse return error.TooManyImages;
    const image = image_pool.get(index) orelse return error.TooManyImages;

    var image_object: u32 = undefined;

    api.glGenTextures(1, &image_object);
    if (debug_api) {
        log.debug("glGenTextures {} {}", .{ 1, image_object });
    }

    if (debug_api) {
        log.debug("glBindTexture {} {}", .{ GL_Enum.GL_TEXTURE_2D, image_object });
    }
    api.glBindTexture(.GL_TEXTURE_2D, image_object);

    const format = gl_image_format(info.format);

    if (info.data) |data| {
        if (debug_api) {
            log.debug("glTexImage2D {} {} {} {} {} {} {} {} {?*}", .{
                GL_Enum.GL_TEXTURE_2D,
                0, // level
                format.internal,
                info.width,
                info.height,
                0, // border
                format.format,
                format.type,
                data,
            });
        }
        api.glTexImage2D(
            .GL_TEXTURE_2D,
            0, // level
            format.internal,
            info.width,
            info.height,
            0, // border
            format.format,
            format.type,
            @ptrCast(data),
        );
    }

    if (debug_api) {
        log.debug("glBindTexture {} {}", .{ GL_Enum.GL_TEXTURE_2D, 0 });
    }
    api.glBindTexture(.GL_TEXTURE_2D, 0);

    image.* = .{
        .label = info.label,
        .object = image_object,
    };

    if (info.label) |label| {
        if (debug_api) {
            log.debug("glObjectLabel {} {} {s}", .{ .GL_TEXTURE, image_object, label });
        }
        api.glObjectLabel(.GL_TEXTURE, image_object, @intCast(label.len), label.ptr);
    }

    return .{
        .handle = index,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn gl_image_format(format: video.PixelFormat) GL_ImageFormat {
    return switch (format) {
        .r8 => return .{ .internal = .GL_RED, .format = .GL_RED, .type = .GL_UNSIGNED_BYTE },
        else => return .{ .internal = .GL_RED, .format = .GL_RED, .type = .GL_UNSIGNED_BYTE },
    };
}

const GL_ImageFormat = struct {
    internal: GL_Enum,
    format: GL_Enum,
    type: GL_Enum,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn apply_image(self: video.Image, a_sampler: video.Sampler) void {
    const index = self.handle;
    const image = image_pool.get(index) orelse return;

    const sampler_index = a_sampler.handle;
    const sampler = sampler_pool.get(sampler_index) orelse return;

    if (debug_api) {
        log.debug("glActiveTexture {}", .{GL_Enum.GL_TEXTURE0});
    }
    api.glActiveTexture(.GL_TEXTURE0);

    if (debug_api) {
        log.debug("glBindTexture {} {}", .{ GL_Enum.GL_TEXTURE_2D, image.object });
    }
    api.glBindTexture(.GL_TEXTURE_2D, image.object);

    if (debug_api) {
        log.debug("glTexParameteri {} {} {}", .{
            GL_Enum.GL_TEXTURE_2D,
            GL_Enum.GL_TEXTURE_MIN_FILTER,
            sampler.min_filter,
        });
    }
    api.glTexParameteri(.GL_TEXTURE_2D, .GL_TEXTURE_MIN_FILTER, sampler.min_filter);

    if (debug_api) {
        log.debug("glTexParameteri {} {} {}", .{
            GL_Enum.GL_TEXTURE_2D,
            GL_Enum.GL_TEXTURE_MAG_FILTER,
            sampler.mag_filter,
        });
    }
    api.glTexParameteri(.GL_TEXTURE_2D, .GL_TEXTURE_MAG_FILTER, sampler.mag_filter);

    if (debug_api) {
        log.debug("glTexParameteri {} {} {}", .{
            GL_Enum.GL_TEXTURE_2D,
            GL_Enum.GL_TEXTURE_WRAP_S,
            sampler.wrap_s,
        });
    }
    api.glTexParameteri(.GL_TEXTURE_2D, .GL_TEXTURE_WRAP_S, sampler.wrap_s);

    if (debug_api) {
        log.debug("glTexParameteri {} {} {}", .{
            GL_Enum.GL_TEXTURE_2D,
            GL_Enum.GL_TEXTURE_WRAP_T,
            sampler.wrap_t,
        });
    }
    api.glTexParameteri(.GL_TEXTURE_2D, .GL_TEXTURE_WRAP_T, sampler.wrap_t);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn delete_image(self: video.Image) void {
    const index = self.handle;
    const image = image_pool.get(index) orelse return;

    api.glDeleteTextures(1, &image.object);

    image_pool.delete(index);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const GL_Sampler = struct {
    label: ?[]const u8 = null,
    min_filter: GL_Enum,
    mag_filter: GL_Enum,
    wrap_s: GL_Enum,
    wrap_t: GL_Enum,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn create_sampler(info: video.CreateSamplerInfo) video.VideoError!video.Sampler {
    const index = sampler_pool.create() orelse return error.TooManySamplers;
    const sampler = sampler_pool.get(index) orelse return error.TooManySamplers;

    sampler.* = .{
        .label = info.label,
        .min_filter = gl_filter(info.min_filter),
        .mag_filter = gl_filter(info.mag_filter),
        .wrap_s = gl_wrap(info.wrap_u),
        .wrap_t = gl_wrap(info.wrap_v),
    };

    return .{
        .handle = index,
    };
}

fn gl_filter(filter: video.SampleFilter) GL_Enum {
    return switch (filter) {
        .nearest => .GL_NEAREST,
        .linear => .GL_LINEAR,
    };
}

fn gl_wrap(wrap: video.SampleWrap) GL_Enum {
    return switch (wrap) {
        .repeat => .GL_REPEAT,
        .clamp_to_edge => .GL_CLAMP_TO_EDGE,
        .mirrored_repeat => .GL_MIRRORED_REPEAT,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn delete_sampler(self: video.Sampler) void {
    const index = self.handle;
    const sampler = sampler_pool.get(index) orelse return;

    _ = sampler;

    sampler_pool.delete(index);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
