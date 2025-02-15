const ng = @import("ng");

pub const vertex_source =
    \\ #version 330 core
    \\
    \\ layout(location=0) in vec3 position;
    \\ layout(location=1) in vec4 color0;
    \\ 
    \\ out vec4 vertex_color;
    \\ 
    \\ void main() {
    \\     gl_Position = vec4(position, 1);
    \\     vertex_color = color0;
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
    position: ng.Vec2,
    color: ng.Color,
};
