///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Vec2 = @Vector(2, f32);
pub const Vec3 = @Vector(3, f32);
pub const Vec4 = @Vector(4, f32);
pub const Mat4 = [16]f32;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const mat4_identity: Mat4 = [16]f32{
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn mat4_translate(x: f32, y: f32, z: f32) Mat4 {
    return .{
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        x, y, z, 1,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn mat4_rotate_z(a: f32) Mat4 {
    const cos_angle = @cos(a);
    const sin_angle = @sin(a);
    return .{
        cos_angle,  sin_angle, 0, 0,
        -sin_angle, cos_angle, 0, 0,
        0,          0,         1, 0,
        0,          0,         0, 1,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn mat4_scale(x: f32, y: f32, z: f32) Mat4 {
    return .{
        x, 0, 0, 0,
        0, y, 0, 0,
        0, 0, z, 0,
        0, 0, 0, 1,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn mat4_mul(a: Mat4, b: Mat4) Mat4 {
    var c: Mat4 = undefined;

    c[0] = a[0] * b[0] + a[1] * b[4] + a[2] * b[8] + a[3] * b[12];
    c[1] = a[0] * b[1] + a[1] * b[5] + a[2] * b[9] + a[3] * b[13];
    c[2] = a[0] * b[2] + a[1] * b[6] + a[2] * b[10] + a[3] * b[14];
    c[3] = a[0] * b[3] + a[1] * b[7] + a[2] * b[11] + a[3] * b[15];

    c[4] = a[4] * b[0] + a[5] * b[4] + a[6] * b[8] + a[7] * b[12];
    c[5] = a[4] * b[1] + a[5] * b[5] + a[6] * b[9] + a[7] * b[13];
    c[6] = a[4] * b[2] + a[5] * b[6] + a[6] * b[10] + a[7] * b[14];
    c[7] = a[4] * b[3] + a[5] * b[7] + a[6] * b[11] + a[7] * b[15];

    c[8] = a[8] * b[0] + a[9] * b[4] + a[10] * b[8] + a[11] * b[12];
    c[9] = a[8] * b[1] + a[9] * b[5] + a[10] * b[9] + a[11] * b[13];
    c[10] = a[8] * b[2] + a[9] * b[6] + a[10] * b[10] + a[11] * b[14];
    c[11] = a[8] * b[3] + a[9] * b[7] + a[10] * b[11] + a[11] * b[15];

    c[12] = a[12] * b[0] + a[13] * b[4] + a[14] * b[8] + a[15] * b[12];
    c[13] = a[12] * b[1] + a[13] * b[5] + a[14] * b[9] + a[15] * b[13];
    c[14] = a[12] * b[2] + a[13] * b[6] + a[14] * b[10] + a[15] * b[14];
    c[15] = a[12] * b[3] + a[13] * b[7] + a[14] * b[11] + a[15] * b[15];

    return c;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn mat4_transform(a: Mat4, v: Vec4) Vec4 {
    var result: Vec4 = undefined;

    result[0] = a[0] * v[0] + a[4] * v[1] + a[8] * v[2] + a[12] * v[3];
    result[1] = a[1] * v[0] + a[5] * v[1] + a[9] * v[2] + a[13] * v[3];
    result[2] = a[2] * v[0] + a[6] * v[1] + a[10] * v[2] + a[14] * v[3];
    result[3] = a[3] * v[0] + a[7] * v[1] + a[11] * v[2] + a[15] * v[3];

    return result;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn mat4_transform2(a: Mat4, v: Vec2) Vec2 {
    var result: Vec2 = undefined;

    result[0] = a[0] * v[0] + a[4] * v[1] + a[12];
    result[1] = a[1] * v[0] + a[5] * v[1] + a[13];

    return result;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn mat4_invert(a: Mat4) Mat4 {
    var c: Mat4 = undefined;

    const b0 = a[0] * a[5] - a[1] * a[4];
    const b1 = a[0] * a[6] - a[2] * a[4];
    const b2 = a[0] * a[7] - a[3] * a[4];
    const b3 = a[1] * a[6] - a[2] * a[5];
    const b4 = a[1] * a[7] - a[3] * a[5];
    const b5 = a[2] * a[7] - a[3] * a[6];
    const b6 = a[8] * a[13] - a[9] * a[12];
    const b7 = a[8] * a[14] - a[10] * a[12];
    const b8 = a[8] * a[15] - a[11] * a[12];
    const b9 = a[9] * a[14] - a[10] * a[13];
    const b10 = a[9] * a[15] - a[11] * a[13];
    const b11 = a[10] * a[15] - a[11] * a[14];

    const det: f32 = 1 / (b0 * b11 - b1 * b10 + b2 * b9 + b3 * b8 - b4 * b7 + b5 * b6);

    c[0] = (a[5] * b11 - a[6] * b10 + a[7] * b9) * det;
    c[1] = (-a[1] * b11 + a[2] * b10 - a[3] * b9) * det;
    c[2] = (a[13] * b5 - a[14] * b4 + a[15] * b3) * det;
    c[3] = (-a[9] * b5 + a[10] * b4 - a[11] * b3) * det;
    c[4] = (-a[4] * b11 + a[6] * b8 - a[7] * b7) * det;
    c[5] = (a[0] * b11 - a[2] * b8 + a[3] * b7) * det;
    c[6] = (-a[12] * b5 + a[14] * b2 - a[15] * b1) * det;
    c[7] = (a[8] * b5 - a[10] * b2 + a[11] * b1) * det;
    c[8] = (a[4] * b10 - a[5] * b8 + a[7] * b6) * det;
    c[9] = (-a[0] * b10 + a[1] * b8 - a[3] * b6) * det;
    c[10] = (a[12] * b4 - a[13] * b2 + a[15] * b0) * det;
    c[11] = (-a[8] * b4 + a[9] * b2 - a[11] * b0) * det;
    c[12] = (-a[4] * b9 + a[5] * b7 - a[6] * b6) * det;
    c[13] = (a[0] * b9 - a[1] * b7 + a[2] * b6) * det;
    c[14] = (-a[12] * b3 + a[13] * b1 - a[14] * b0) * det;
    c[15] = (a[8] * b3 - a[9] * b1 + a[10] * b0) * det;

    return c;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn ortho(width: f32, height: f32) Mat4 {
    return .{
        2 / width, 0,           0,  0,
        0,         -2 / height, 0,  0,
        0,         0,           -2, 0,
        -1,        1,           1,  1,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Camera2D = struct {
    target: Vec2 = .{ 0, 0 },
    origin: Vec2 = .{ 0, 0 },
    rotate: f32 = 0,
    zoom: f32 = 0,

    pub fn identity() Camera2D {
        return .{
            .target = .{ 0, 0 },
            .origin = .{ 0, 0 },
            .rotate = 0,
            .zoom = 0.5,
        };
    }

    pub fn get_matrix(self: Camera2D) Mat4 {
        const target = mat4_translate(self.target[0], self.target[1], 0);
        const rotate = mat4_rotate_z(self.rotate);
        const zoom = mat4_scale(self.zoom, -self.zoom, 1);
        const origin = mat4_translate(-self.origin[0], -self.origin[1], 0);
        return mat4_mul(mat4_mul(origin, mat4_mul(zoom, rotate)), target);
    }

    pub fn to_world(self: Camera2D, pos: Vec2) Vec2 {
        const mat = mat4_invert(self.get_matrix());
        return mat4_transform2(mat, pos);
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
