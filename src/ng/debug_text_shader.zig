const ng = @import("ng");

pub const label = "Debug Text Shader";

pub const vertex_source =
    \\ #version 430 core
    \\
    \\ layout(location=0) in vec2 pos;
    \\ layout(location=1) in vec2 uv;
    \\ layout(location=2) in vec4 col;
    \\
    \\ uniform mat4 mvp;
    \\ 
    \\ layout(location=0) out vec2 vertex_uv;
    \\ layout(location=1) out vec4 vertex_color;
    \\ 
    \\ void main() {
    \\     gl_Position = mvp * vec4(pos, 0, 1);
    \\     vertex_uv = uv;
    \\     vertex_color = col;
    \\ }
;

pub const fragment_source =
    \\ #version 430 core
    \\
    \\ uniform sampler2D smp;
    \\
    \\ layout(location=0) in vec2 vertex_uv;
    \\ layout(location=1) in vec4 vertex_color;
    \\
    \\ layout(location=0) out vec4 frag_color;
    \\
    \\ void main() {
    \\     float r = texture(smp, vertex_uv).r;
    \\     if (r > 0.9) {
    \\         frag_color = vec4 (vertex_color.rgb * r, 1);
    \\     } else {
    \\         frag_color = vec4 (0, 0, 0, r);
    \\     }
    \\ }
;

pub const Vertex = extern struct {
    pos: ng.Vec2,
    uv: ng.Vec2,
    col: ng.Color,
};

pub const Uniforms = extern struct {
    mvp: ng.Mat4,
    smp: ng.Sampler2D,
};

pub const num_samplers = 1;
