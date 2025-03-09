///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

const ng = @import("ng");

const log = ng.Logger(.ecs);

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "saving simple" {
    ng.ecs.init(std.testing.allocator);
    defer ng.ecs.deinit();

    const One = struct {
        one: u8,
        two: u16,
    };

    const Position = struct {
        pos: ng.Vec2,
    };

    const Velocity = struct {
        vel: ng.Vec2,
    };

    const MyKind = enum(u8) {
        a,
        b,
        c,
        d,
        e,
        f,
        g,
    };

    const Alltypes = struct {
        a: bool,
        b: bool,
        c: u8,
        d: u16,
        e: u32,
        f: u64,
        g: i8,
        h: i16,
        i: i32,
        j: i64,
        k: f32,
        l: f32,
        m: ng.Vec2,
        n: ng.Entity,
    };

    ng.register_component("One", One);
    ng.register_component("Position", Position);
    ng.register_component("Velocity", Velocity);
    ng.register_component("MyKind", MyKind);
    ng.register_component("Alltypes", Alltypes);

    const e0 = ng.new();
    e0.set(One{ .one = 1, .two = 2 });
    e0.set(Position{ .pos = .{ 1, 2 } });
    e0.set(Velocity{ .vel = .{ 3, 4 } });
    e0.set(MyKind.c);

    const e1 = ng.new();
    const e2 = ng.new();
    e1.set(Alltypes{
        .a = true,
        .b = false,
        .c = 3,
        .d = 4,
        .e = 5,
        .f = 6,
        .g = 7,
        .h = 8,
        .i = 9,
        .j = 10,
        .k = 11,
        .l = 12,
        .m = ng.Vec2{ 13, 14 },
        .n = e2,
    });

    const memory = try ng.ecs.save(std.testing.allocator);
    defer std.testing.allocator.free(memory);

    const cwd = std.fs.cwd();
    const file = try cwd.createFile("autosave.dat", .{});
    defer file.close();
    _ = try file.write(memory);

    var buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer buffer.deinit();
    const writer = buffer.writer();

    try dump_save(memory, writer);

    try std.testing.expectEqualStrings(
        \\E(00:000000)
        \\  One.one = 1
        \\  One.two = 2
        \\  Position.pos = Vec2(1.000,2.000)
        \\  Velocity.vel = Vec2(3.000,4.000)
        \\  MyKind = .c
        \\E(00:000001)
        \\  Alltypes.a = true
        \\  Alltypes.b = false
        \\  Alltypes.c = 3
        \\  Alltypes.d = 4
        \\  Alltypes.e = 5
        \\  Alltypes.f = 6
        \\  Alltypes.g = 7
        \\  Alltypes.h = 8
        \\  Alltypes.i = 9
        \\  Alltypes.j = 10
        \\  Alltypes.k = 11.000
        \\  Alltypes.l = 12.000
        \\  Alltypes.m = Vec2(13.000,14.000)
        \\  Alltypes.n = E(00:000002)
        \\E(00:000002)
        \\
    ,
        buffer.items,
    );
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const HEAD = 0x44414548;
const RCYC = 0x43594352;
const OLDR = 0x52444c4f;
const STR_ = 0x3a525453;
const COMP = 0x504d4f43;
const ENTT = 0x54544e45;

const max_fields = 16;
const max_values = 32;

const ComponentInfo = struct {
    name: []const u8,
    size: usize,
    num_fields: usize,
    fields: [max_fields]struct {
        name: []const u8,
        size: usize,
        offset: usize,
        kind: []const u8,
    },
    tag_type: usize,
    num_values: usize,
    values: [max_values]struct {
        name: []const u8,
        value: usize,
    },
};

const Kinds = std.AutoHashMap(u64, []const u8);

fn dump_save(memory: []const u8, writer: anytype) !void {
    var offset: usize = 0;
    var strings: []const u8 = undefined;
    var kinds = Kinds.init(std.testing.allocator);
    defer kinds.deinit();

    var components = std.AutoHashMap(u64, ComponentInfo).init(std.testing.allocator);
    defer components.deinit();

    while (offset < memory.len) {
        const start_offset = offset;
        const block_length, offset = read_u32(memory, offset);
        const next_offset = start_offset + block_length;

        const block_tag, offset = read_u32(memory, offset);

        switch (block_tag) {
            HEAD => {
                const strings_offset, offset = read_u32(memory, offset);
                const strings_data_length, _ = read_u32(memory, strings_offset);
                strings = memory[strings_offset + 8 .. strings_offset + strings_data_length];
                while (offset < next_offset) {
                    const kind, offset = read_int(memory, offset);
                    const kind_name, offset = read_string(memory, offset, strings);
                    try kinds.put(kind, kind_name);
                }
            },
            RCYC => {},
            OLDR => {},
            STR_ => {},
            COMP => {
                var info: ComponentInfo = undefined;
                const typeid, offset = read_int(memory, offset);
                info.name, offset = read_string(memory, offset, strings);
                info.size, offset = read_int(memory, offset);
                const info_kind, offset = read_int(memory, offset);
                if (info_kind == 0) // struct
                {
                    info.num_values = 0;
                    info.num_fields, offset = read_int(memory, offset);
                    for (0..info.num_fields) |i| {
                        info.fields[i].name, offset = read_string(memory, offset, strings);
                        info.fields[i].offset, offset = read_int(memory, offset);
                        info.fields[i].size, offset = read_int(memory, offset);
                        const field_kind, offset = read_int(memory, offset);
                        info.fields[i].kind = kinds.get(field_kind).?;
                    }
                    try components.put(typeid, info);
                } else if (info_kind == 1) // enum
                {
                    info.num_fields = 0;
                    info.tag_type, offset = read_int(memory, offset);
                    info.num_values, offset = read_int(memory, offset);
                    for (0..info.num_values) |i| {
                        info.values[i].value, offset = read_int(memory, offset);
                        info.values[i].name, offset = read_string(memory, offset, strings);
                    }
                    try components.put(typeid, info);
                }
            },
            ENTT => {
                const entity = std.mem.bytesToValue(ng.Entity, &memory[offset]);
                offset += 4;
                try writer.print("{}\n", .{entity});
                while (offset < next_offset) {
                    const typeid, offset = read_int(memory, offset);
                    const length, offset = read_int(memory, offset);
                    const data = memory[offset .. offset + length];
                    const component = components.get(typeid).?;
                    offset += length;
                    try print_component(component, data, &kinds, writer);
                }
            },
            else => {
                const block_tag_name, _ = read_tag(memory, start_offset + 4);
                try writer.print("Unknown tag {s} {x}\n", .{ block_tag_name, block_tag });
            },
        }
        offset = next_offset;
    }
}

fn print_component(
    component: ComponentInfo,
    data: []const u8,
    kinds: *const Kinds,
    writer: anytype,
) !void {
    if (component.num_values > 0) {
        const tag_type = kinds.get(component.tag_type).?;
        if (std.mem.eql(u8, tag_type, "u8")) {
            const value = data[0];
            var maybe_name: ?[]const u8 = null;
            for (0..component.num_values) |i| {
                if (component.values[i].value == value) {
                    maybe_name = component.values[i].name;
                }
            }
            if (maybe_name) |name| {
                try writer.print("  {s} = .{s}\n", .{ component.name, name });
            } else {
                try writer.print("  {s} = {}\n", .{ component.name, value });
            }
        }
    } else {
        for (0..component.num_fields) |i| {
            const field = component.fields[i];
            if (std.mem.eql(u8, "bool", field.kind)) {
                const value = std.mem.bytesToValue(bool, &data[field.offset]);
                try writer.print("  {s}.{s} = {}\n", .{
                    component.name,
                    field.name,
                    value,
                });
            } else if (std.mem.eql(u8, "u8", field.kind)) {
                const value = std.mem.bytesToValue(u8, &data[field.offset]);
                try writer.print("  {s}.{s} = {}\n", .{
                    component.name,
                    field.name,
                    value,
                });
            } else if (std.mem.eql(u8, "u16", field.kind)) {
                const value = std.mem.bytesToValue(u16, &data[field.offset]);
                try writer.print("  {s}.{s} = {}\n", .{
                    component.name,
                    field.name,
                    value,
                });
            } else if (std.mem.eql(u8, "u32", field.kind)) {
                const value = std.mem.bytesToValue(u32, &data[field.offset]);
                try writer.print("  {s}.{s} = {}\n", .{
                    component.name,
                    field.name,
                    value,
                });
            } else if (std.mem.eql(u8, "u64", field.kind)) {
                const value = std.mem.bytesToValue(u64, &data[field.offset]);
                try writer.print("  {s}.{s} = {}\n", .{
                    component.name,
                    field.name,
                    value,
                });
            } else if (std.mem.eql(u8, "i8", field.kind)) {
                const value = std.mem.bytesToValue(i8, &data[field.offset]);
                try writer.print("  {s}.{s} = {}\n", .{
                    component.name,
                    field.name,
                    value,
                });
            } else if (std.mem.eql(u8, "i16", field.kind)) {
                const value = std.mem.bytesToValue(i16, &data[field.offset]);
                try writer.print("  {s}.{s} = {}\n", .{
                    component.name,
                    field.name,
                    value,
                });
            } else if (std.mem.eql(u8, "i32", field.kind)) {
                const value = std.mem.bytesToValue(i32, &data[field.offset]);
                try writer.print("  {s}.{s} = {}\n", .{
                    component.name,
                    field.name,
                    value,
                });
            } else if (std.mem.eql(u8, "i64", field.kind)) {
                const value = std.mem.bytesToValue(i64, &data[field.offset]);
                try writer.print("  {s}.{s} = {}\n", .{
                    component.name,
                    field.name,
                    value,
                });
            } else if (std.mem.eql(u8, "f32", field.kind)) {
                const value = std.mem.bytesToValue(f32, &data[field.offset]);
                try writer.print("  {s}.{s} = {d:0.3}\n", .{
                    component.name,
                    field.name,
                    value,
                });
            } else if (std.mem.eql(u8, "f64", field.kind)) {
                const value = std.mem.bytesToValue(f64, &data[field.offset]);
                try writer.print("  {s}.{s} = {d:0.6}\n", .{
                    component.name,
                    field.name,
                    value,
                });
            } else if (std.mem.eql(u8, "Vec2", field.kind)) {
                const value = std.mem.bytesToValue(ng.Vec2, &data[field.offset]);
                try writer.print("  {s}.{s} = Vec2({d:0.3},{d:0.3})\n", .{
                    component.name,
                    field.name,
                    value[0],
                    value[1],
                });
            } else if (std.mem.eql(u8, "Entity", field.kind)) {
                const value = std.mem.bytesToValue(ng.Entity, &data[field.offset]);
                try writer.print("  {s}.{s} = {}\n", .{
                    component.name,
                    field.name,
                    value,
                });
            } else {
                try writer.print("  {s}.{s} = {s} {any}\n", .{
                    component.name,
                    field.name,
                    field.kind,
                    data,
                });
            }
        }
    }
}

fn read_int(memory: []const u8, offset: usize) struct { u64, usize } {
    if (memory[offset] & 0x80 == 0x00) {
        return .{ memory[offset], offset + 1 };
    } else if (memory[offset] & 0xC0 == 0x80) {
        var data: [2]u8 = undefined;
        data[1] = memory[offset + 0];
        data[0] = memory[offset + 1];
        const ptr: *u16 = @alignCast(@ptrCast(&data));
        return .{ ptr.*, offset + 2 };
    } else if (memory[offset] & 0xE0 == 0xC0) {
        var data: [4]u8 = undefined;
        data[3] = memory[offset + 0];
        data[2] = memory[offset + 1];
        data[1] = memory[offset + 2];
        data[0] = memory[offset + 3];
        const ptr: *u32 = @alignCast(@ptrCast(&data));
        return .{ ptr.*, offset + 4 };
    } else if (memory[offset] & 0xFF == 0xF4) {
        var data: [4]u8 = undefined;
        data[3] = memory[offset + 0];
        data[2] = memory[offset + 1];
        data[1] = memory[offset + 2];
        data[0] = memory[offset + 3];
        const ptr: *u32 = @alignCast(@ptrCast(&data));
        return .{ ptr.*, offset + 4 };
    } else if (memory[offset] & 0xFF == 0xF8) {
        var data: [8]u8 = undefined;
        data[7] = memory[offset + 0];
        data[6] = memory[offset + 1];
        data[5] = memory[offset + 2];
        data[4] = memory[offset + 3];
        data[3] = memory[offset + 4];
        data[2] = memory[offset + 5];
        data[1] = memory[offset + 6];
        data[0] = memory[offset + 7];
        const ptr: *u64 = @alignCast(@ptrCast(&data));
        return .{ ptr.*, offset + 8 };
    }
    return .{ 0, offset + 1 };
}

fn read_u32(memory: []const u8, offset: usize) struct { u32, usize } {
    const value = std.mem.bytesToValue(u32, &memory[offset]);
    return .{ value, offset + 4 };
}

fn read_string(memory: []const u8, off: usize, strings: []const u8) struct { []u8, usize } {
    var offset = off;
    const string_offset, offset = read_int(memory, offset);
    const string_length, offset = read_int(memory, offset);
    var data: []u8 = undefined;
    data.ptr = @constCast(@ptrCast(&strings[string_offset]));
    data.len = string_length;
    return .{ data, offset };
}

fn read_tag(memory: []const u8, offset: usize) struct { [4]u8, usize } {
    var data: [4]u8 = undefined;
    data[0] = memory[offset + 0];
    data[1] = memory[offset + 1];
    data[2] = memory[offset + 2];
    data[3] = memory[offset + 3];
    return .{ data, offset + 4 };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "benchmark entity creation" {
    ng.ecs.init(std.testing.allocator);
    defer ng.ecs.deinit();

    const test_size = 1_000_000;
    const run_size = 10;

    var entities: [test_size]ng.Entity = undefined;
    var timings: [run_size]f64 = undefined;

    for (0..run_size) |run| {
        const start_time = std.time.nanoTimestamp();
        for (0..test_size) |i| {
            entities[i] = ng.new();
        }
        const end_time = std.time.nanoTimestamp();
        for (0..test_size) |i| {
            entities[i].delete();
        }
        const elapsed_time = end_time - start_time;
        const time: f64 = @floatFromInt(elapsed_time);

        timings[run] = time;
        ng.ecs.recycle_old_generations();
    }

    var total_time: f64 = 0;
    for (1..run_size) |run| {
        total_time += timings[run];
    }
    total_time = total_time / (run_size - 1) / test_size;
    std.debug.print("ecs create {d:0.3} ns\n", .{total_time});

    for (0..run_size) |run| {
        for (0..test_size) |i| {
            entities[i] = ng.new();
        }
        const start_time = std.time.nanoTimestamp();
        for (0..test_size) |i| {
            entities[i].delete();
        }
        const end_time = std.time.nanoTimestamp();
        const elapsed_time = end_time - start_time;
        const time: f64 = @floatFromInt(elapsed_time);

        timings[run] = time;
        ng.ecs.recycle_old_generations();
    }

    total_time = 0;
    for (1..run_size) |run| {
        total_time += timings[run];
    }
    total_time = total_time / (run_size - 1) / test_size;
    std.debug.print("ecs delete {d:0.3} ns\n", .{total_time});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
