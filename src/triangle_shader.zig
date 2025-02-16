const ng = @import("ng");

pub const label = "Triangle Shader";

pub const vertex_source =
    \\ #version 330 core
    \\
    \\ layout(location=0) in vec2 pos;
    \\ layout(location=1) in vec4 col;
    \\ 
    \\ out vec4 vertex_color;
    \\ 
    \\ void main() {
    \\     gl_Position = vec4(pos, 0, 1);
    \\     vertex_color = col;
    \\ }
;

pub const fragment_source =
    \\ #version 330 core
    \\
    \\ in vec4 vertex_color;
    \\ out vec4 frag_color;
    \\
    \\ void main() {
    \\     frag_color = vertex_color;
    \\ }
;

pub const Vertex = extern struct {
    pos: ng.Vec2,
    col: ng.Color,
};
