#version 430 core

const vec4 positions[6] = {
    vec4(-1.0, -1.0, 0.0, 1.0),
    vec4(1.0, -1.0, 0.0, 1.0),
    vec4(-1.0, 1.0, 0.0, 1.0),
    vec4(-1.0, 1.0, 0.0, 1.0),
    vec4(1.0, -1.0, 0.0, 1.0),
    vec4(1.0, 1.0, 0.0, 1.0),
};

out vec2 frag_coord;
out float view_size;

uniform mat4 mvp;

void main() {
    vec4 position = positions[gl_VertexID];
    mat4 inv_mvp = inverse(mvp);
    vec4 world_pos = inv_mvp * position;
    frag_coord.xy = world_pos.xy;
    gl_Position = position;
    vec4 tl = inv_mvp * positions[0];
    vec4 br = inv_mvp * positions[5];
    vec4 wh = abs (tl - br);
    view_size = min (wh.x, wh.y);
}
