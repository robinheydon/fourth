#version 430 core

in vec2 frag_coord;
in float view_size;

out vec4 frag_color;

float grid (vec2 position, float scale, float width) {
    vec2 coord = position / scale;
    vec2 derivative = fwidth (coord);
    vec2 grid = 1 - abs (fract (coord - 0.5) - 0.5) / derivative / width;
    vec2 step = smoothstep (0.0, 0.1, grid);
    return max (step.x, step.y);
}

void main() {
    float w0 = clamp (1 - view_size / 100, 1.0, 4.0);
    float w1 = clamp (1 - view_size / 1000, 1.0, 4.0);
    float w2 = clamp (1 - view_size / 10000, 1.0, 4.0);
    float w3 = clamp (1 - view_size / 100000, 1.0, 4.0);
    float w4 = clamp (1 - view_size / 1000000, 1.0, 4.0);
    float l0 = grid (frag_coord, 1.0, w0);
    float l1 = grid (frag_coord, 10.0, w1);
    float l2 = grid (frag_coord, 100.0, w2);
    float l3 = grid (frag_coord, 1000.0, w3);
    float l4 = grid (frag_coord, 10000.0, w4);

    const float scale = 0.3;

    float l = l0 * clamp (scale - view_size / 100, 0.0, scale) +
              l1 * clamp (scale - view_size / 1000, 0.0, scale) +
              l2 * clamp (scale - view_size / 10000, 0.0, scale) +
              l3 * clamp (scale - view_size / 100000, 0.0, scale) +
              l4 * clamp (scale - view_size / 1000000, 0.0, scale);

    vec4 grid_color = vec4 (0.3, 0.3, 0.4, 1.0);
    frag_color = vec4 (grid_color.rgb, l);
}
