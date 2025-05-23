///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");
const builtin = @import("builtin");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const ng = @import("ng.zig");
const event = @import("event.zig");
const x11 = @import("x11.zig");
const color = @import("color.zig");
const math = @import("math.zig");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Event = event.Event;
pub const Color = color.Color;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const log = ng.Logger(.ng_video);

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

var initialized = false;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const VideoError = error{
    CannotCreateShader,
    CannotOpenWindow,
    InvalidCreateInfo,
    InvalidShaderIndex,
    TooManyBindings,
    TooManyBuffers,
    TooManyImages,
    TooManyPipelines,
    TooManySamplers,
    TooManyShaders,
    UnknownVertexAttribute,
};

pub const Platform = struct {
    deinit: *const fn () void,

    create_window: *const fn (CreateWindowOptions) VideoError!Window,
    close_window: *const fn (Window) void,
    get_window_size: *const fn (Window) ng.Vec2,
    set_window_size: *const fn (Window, ng.Vec2) void,
    set_swap_interval: *const fn (Window, SwapInterval) void,
    toggle_fullscreen: *const fn (Window) void,
    acknowledge_resize: *const fn (Window) void,
    use_cursor: *const fn (Cursor) void,

    acquire_command_buffer: *const fn (Window) VideoError!CommandBuffer,
    submit_command_buffer: *const fn (CommandBuffer) VideoError!void,
    acquire_swapchain_texture: *const fn (CommandBuffer) VideoError!GPUTexture,

    begin_render_pass: *const fn (CommandBuffer, BeginRenderPassInfo) VideoError!RenderPass,
    end_render_pass: *const fn (RenderPass) void,
    apply_pipeline: *const fn (RenderPass, Pipeline) void,
    apply_bindings: *const fn (RenderPass, Binding) void,
    apply_uniform: *const fn (RenderPass, UniformInfo) void,
    apply_scissor: *const fn (RenderPass, ng.Vec2, ng.Vec2) void,
    draw: *const fn (RenderPass, usize, usize) void,
    get_render_pass_size: *const fn (RenderPass) ng.Vec2,

    create_shader: *const fn (CreateShaderInfo) VideoError!Shader,
    delete_shader: *const fn (Shader) void,

    create_buffer: *const fn (CreateBufferInfo) VideoError!Buffer,
    update_buffer: *const fn (Buffer, data: []const u8) void,
    delete_buffer: *const fn (Buffer) void,

    create_pipeline: *const fn (CreatePipelineInfo) VideoError!Pipeline,
    delete_pipeline: *const fn (Pipeline) void,

    create_binding: *const fn (CreateBindingInfo) VideoError!Binding,
    delete_binding: *const fn (Binding) void,

    create_image: *const fn (CreateImageInfo) VideoError!Image,
    delete_image: *const fn (Image) void,

    create_sampler: *const fn (CreateSamplerInfo) VideoError!Sampler,
    delete_sampler: *const fn (Sampler) void,

    generate_events: *const fn () void,
};

var platform: Platform = undefined;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn init() !void {
    if (x11.init()) |plat| {
        platform = plat;
    } else |_| {
        return error.NoVideoPlatform;
    }

    initialized = true;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn deinit() void {
    if (initialized) {
        platform.deinit();
    }
    initialized = false;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Window = struct {
    handle: usize,

    pub fn close(self: Window) void {
        platform.close_window(self);
    }

    pub fn set_swap_interval(self: Window, interval: SwapInterval) void {
        platform.set_swap_interval(self, interval);
    }

    pub fn get_size(self: Window) ng.Vec2 {
        return platform.get_window_size(self);
    }

    pub fn set_size(self: Window, size: ng.Vec2) void {
        platform.set_window_size(self, size);
    }

    pub fn acquire_command_buffer(self: Window) !CommandBuffer {
        return platform.acquire_command_buffer(self);
    }

    pub fn toggle_fullscreen(self: Window) void {
        platform.toggle_fullscreen(self);
    }

    pub fn acknowledge_resize(self: Window) void {
        platform.acknowledge_resize(self);
    }
};

pub const WindowSize = struct {
    width: f32,
    height: f32,
};

pub const SwapInterval = enum {
    fast,
    lowpower,
    vsync,
    double,
    adaptive,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Cursor = enum {
    default,
    move,
    resize,
    resize_ns,
    resize_ew,
};

pub fn use_cursor(cursor: Cursor) void {
    platform.use_cursor(cursor);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn create_window(options: CreateWindowOptions) !Window {
    if (!initialized) return error.VideoNotInitialized;

    return platform.create_window(options);
}

pub const CreateWindowOptions = struct {
    name: ?[*:0]const u8 = null,
    width: ?u32 = null,
    height: ?u32 = null,
    resizable: bool = false,
    fullscreen: bool = false,
    gl_major_version: u32 = 4,
    gl_minor_version: u32 = 6,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn generate_events() void {
    platform.generate_events();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const CommandBuffer = struct {
    handle: usize,

    pub fn acquire_swapchain_texture(self: CommandBuffer) VideoError!GPUTexture {
        return platform.acquire_swapchain_texture(self);
    }

    pub fn begin_render_pass(
        self: CommandBuffer,
        info: BeginRenderPassInfo,
    ) VideoError!RenderPass {
        return platform.begin_render_pass(self, info);
    }

    pub fn submit(self: CommandBuffer) VideoError!void {
        return platform.submit_command_buffer(self);
    }
};

pub const BeginRenderPassInfo = struct {
    texture: GPUTexture,
    clear_color: Color = .black,
    load: LoadOp = .clear,
    store: StoreOp = .store,
};

const LoadOp = enum {
    clear,
    load,
    dont_care,
};

const StoreOp = enum {
    store,
    dont_care,
    resolve,
    resolve_and_store,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const GPUTexture = struct {
    handle: usize,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const RenderPass = struct {
    handle: usize,

    pub fn end(self: RenderPass) void {
        platform.end_render_pass(self);
    }

    pub fn apply_pipeline(self: RenderPass, pipeline: Pipeline) void {
        platform.apply_pipeline(self, pipeline);
    }

    pub fn apply_bindings(self: RenderPass, bindings: Binding) void {
        platform.apply_bindings(self, bindings);
    }

    pub fn apply_uniform_mat4(self: RenderPass, index: anytype, data: math.Mat4) void {
        const info: UniformInfo = .{
            .index = @intFromEnum(index),
            .name = @tagName(index),
            .kind = .mat4,
            .data = ng.as_bytes(&data),
        };
        platform.apply_uniform(self, info);
    }

    pub fn apply_uniform_u32(self: RenderPass, index: anytype, data: u32) void {
        const info: UniformInfo = .{
            .index = @intFromEnum(index),
            .name = @tagName(index),
            .kind = .u32,
            .data = ng.as_bytes(&data),
        };
        platform.apply_uniform(self, info);
    }

    pub fn apply_scissor(self: RenderPass, pos: ng.Vec2, size: ng.Vec2) void {
        platform.apply_scissor(self, pos, size);
    }

    pub fn get_size(self: RenderPass) ng.Vec2 {
        return platform.get_render_pass_size(self);
    }

    pub fn draw(self: RenderPass, num_vertexes: usize) void {
        platform.draw(self, 0, num_vertexes);
    }

    pub fn draw_subset(self: RenderPass, start: usize, count: usize) void {
        platform.draw(self, start, count);
    }
};

pub const UniformInfo = struct {
    index: u32,
    name: [*:0]const u8,
    kind: UniformKind,
    data: []const u8,
};

pub const UniformKind = enum {
    u32,
    mat4,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const max_vertex_attributes = 32;

pub const CreateShaderInfo = struct {
    label: ?[]const u8,
    vertex_source: [*:0]const u8,
    fragment_source: [*:0]const u8,

    vertex_attrib: [max_vertex_attributes]VertexAttribute =
        .{VertexAttribute{}} ** max_vertex_attributes,
};

pub const VertexAttribute = struct {
    size: usize = 0,
    vertex_type: VertexType = .unknown,
    normalize: bool = false,
    stride: usize = 0,
    offset: usize = 0,
};

pub const VertexType = enum(u32) {
    unknown,
    i8 = 0x1400,
    u8 = 0x1401,
    i16 = 0x1402,
    u16 = 0x1403,
    i32 = 0x1404,
    u32 = 0x1405,
    f32 = 0x1406,
    f64 = 0x140A,
    f16 = 0x140B,
};

pub const Shader = struct {
    handle: usize,

    pub fn delete(self: Shader) void {
        platform.delete_shader(self);
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn create_shader(shader: anytype) VideoError!Shader {
    var info: CreateShaderInfo = .{
        .label = shader.label,
        .vertex_source = shader.vertex_source,
        .fragment_source = shader.fragment_source,
    };

    if (@hasDecl(shader, "Vertex")) {
        try build_vertex_attributes(&info, shader.Vertex);
    }

    return platform.create_shader(info);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn build_vertex_attributes(info: *CreateShaderInfo, T: type) !void {
    const t_size = @sizeOf(T);
    inline for (0.., std.meta.fields(T)) |i, field| {
        var attr = try attribute_info(field.type);
        attr.stride = t_size;
        attr.offset = @offsetOf(T, field.name);

        info.vertex_attrib[i] = attr;
    }
}

fn attribute_info(comptime T: type) !VertexAttribute {
    switch (T) {
        u8 => return .{
            .size = 1,
            .vertex_type = .u8,
        },
        i8 => return .{
            .size = 1,
            .vertex_type = .i8,
        },
        Color => return .{
            .size = 4,
            .vertex_type = .u8,
            .normalize = true,
        },
        f32 => return .{
            .size = 1,
            .vertex_type = .f32,
        },
        ng.Vec2, [2]f32 => return .{
            .size = 2,
            .vertex_type = .f32,
        },
        ng.Vec3, [3]f32 => return .{
            .size = 3,
            .vertex_type = .f32,
        },
        ng.Vec4, [4]f32 => return .{
            .size = 4,
            .vertex_type = .f32,
        },
        [16]f32 => return .{
            .size = 16,
            .vertex_type = .f32,
        },
        else => {},
    }
    log.fatal("Unknown vertex attribute type : {s}\n", .{@typeName(T)});
    return error.UnknownVertexAttribute;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const CreateBufferInfo = struct {
    label: ?[]const u8,
    kind: BufferKind = .vertex,
    data: ?[]const u8 = null,
    size: ?usize = null,
    update: BufferUpdate = .static,
};

pub const BufferKind = enum {
    vertex,
    index,
};

pub const BufferUpdate = enum {
    static,
    stream,
    dynamic,
};

pub const Buffer = struct {
    handle: usize,

    pub fn update(self: Buffer, data: []const u8) void {
        platform.update_buffer(self, data);
    }

    pub fn delete(self: Buffer) void {
        platform.delete_buffer(self);
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn create_buffer(info: CreateBufferInfo) !Buffer {
    return platform.create_buffer(info);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const CreatePipelineInfo = struct {
    label: ?[]const u8 = null,
    shader: Shader,
    primitive: Primitive,
    index_type: IndexType = .none,
    blend: BlendInfo = .{},
    scissor_test: bool = false,
};

pub const Primitive = enum {
    triangle_list,
    triangle_strip,
    line_list,
    line_strip,
    point_list,
};

pub const IndexType = enum {
    none,
    u16,
    u32,
};

pub const BlendInfo = struct {
    enabled: bool = false,
    src_factor_rgb: BlendFactor = .one,
    dst_factor_rgb: BlendFactor = .zero,
    src_factor_alpha: BlendFactor = .one,
    dst_factor_alpha: BlendFactor = .zero,
};

pub const BlendFactor = enum {
    one,
    zero,
    one_minus_src_alpha,
    src_alpha,
};

pub const Pipeline = struct {
    handle: usize,

    pub fn delete(self: Pipeline) void {
        platform.delete_pipeline(self);
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn create_pipeline(info: CreatePipelineInfo) !Pipeline {
    return platform.create_pipeline(info);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const CreateBindingInfo = struct {
    label: ?[]const u8 = null,
    vertex_buffers: ?[]const Buffer = null,
    index_buffers: ?[]const Buffer = null,
    image: ?Image = null,
    sampler: ?Sampler = null,
};

pub const Binding = struct {
    handle: usize,

    pub fn delete(self: Binding) void {
        platform.delete_binding(self);
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn create_binding(info: CreateBindingInfo) !Binding {
    return platform.create_binding(info);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const CreateImageInfo = struct {
    label: ?[]const u8 = null,
    width: u32,
    height: u32,
    format: PixelFormat,
    data: ?[]const u8 = null,
};

pub const PixelFormat = enum {
    unknown,
    none,
    r8,
    r8sn,
    r8ui,
    r8si,

    r16,
    r16sn,
    r16ui,
    r16si,
    r16f,

    rg8,
    rg8sn,
    rg8ui,
    rg8si,

    r32ui,
    r32si,
    r32f,
    rg16,
    rg16sn,
    rg16ui,
    rg16si,
    rg16f,

    rgba8,
    srgb8a8,
    rgba8sn,
    rgba8ui,

    bgra8,
    bgr10a2,
    rg11b10f,
    rgb9e5,

    rg32ui,
    rg32si,
    rg32f,

    rgba16,
    rgba16sn,
    rgba16ui,
    rgba16si,
    rgba16f,

    rgba32ui,
    rgba32si,
    rgba32f,

    depth,
    depth_stencil,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Image = struct {
    handle: usize,

    pub fn delete(self: Image) void {
        platform.delete_image(self);
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn create_image(info: CreateImageInfo) !Image {
    return platform.create_image(info);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const CreateSamplerInfo = struct {
    label: ?[]const u8 = null,
    min_filter: SampleFilter = .nearest,
    mag_filter: SampleFilter = .nearest,
    wrap_u: SampleWrap = .clamp_to_edge,
    wrap_v: SampleWrap = .clamp_to_edge,
};

pub const SampleFilter = enum {
    nearest,
    linear,
};

pub const SampleWrap = enum {
    repeat,
    clamp_to_edge,
    mirrored_repeat,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Sampler = struct {
    handle: usize,

    pub fn delete(self: Sampler) void {
        platform.delete_sampler(self);
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn create_sampler(info: CreateSamplerInfo) !Sampler {
    return platform.create_sampler(info);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Sampler2D = struct {};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
