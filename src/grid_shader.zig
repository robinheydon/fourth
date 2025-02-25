const ng = @import("ng");

pub const label = "Grid Shader";

pub const vertex_source = @embedFile("grid_vertex.glsl");

pub const fragment_source = @embedFile("grid_fragment.glsl");

pub const Uniforms = extern struct {
    mvp: ng.Mat4,
};

pub const num_samplers = 0;
