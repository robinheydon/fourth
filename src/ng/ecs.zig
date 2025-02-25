///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");

const ng = @import("ng");

const log = ng.Logger(.ecs);

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

var gpa: std.heap.GeneralPurposeAllocator(.{}) = undefined;
var allocator: std.mem.Allocator = undefined;

var initialized = false;

var next_entity_index: usize = 0;

var generations: std.ArrayList(EntityGeneration) = undefined;
var recycled: std.ArrayList(EntityIndex) = undefined;

var components: std.AutoHashMap(TypeId, ComponentInfo) = undefined;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const EntityIndex = u24;
const EntityGeneration = u8;

pub const Entity = enum(u32) {
    null_entity = 0xFFFFFFFF,
    _,

    pub fn set(self: Entity, comptime Component: type, value: Component) void {
        if (!self.is_valid()) {
            log.err("set invalid entity {}", .{self});
            return;
        }

        const typeid = get_type_id(Component);

        if (components.get(typeid)) |info| {
            log.debug("set {} {s} {any}", .{ self, info.name, value });
        } else {
            log.err("Component {s} not registered", .{@typeName(Component)});
        }
    }

    pub fn delete(self: Entity) void {
        if (!self.is_valid()) {
            log.err("delete invalid entity {}", .{self});
            return;
        }

        log.debug("delete {}", .{self});

        const gen = get_generation(self);
        const idx = get_index(self);

        generations.items[idx] = gen + 1;

        recycled.append(idx) catch |err| {
            log.err("delete entity failed {}", .{err});
            return;
        };
    }

    pub fn is_valid(self: Entity) bool {
        const gen = get_generation(self);
        const idx = get_index(self);

        if (idx >= generations.items.len) {
            return false;
        }
        if (generations.items[idx] != gen) {
            return false;
        }
        return true;
    }

    pub fn format(self: Entity, _: anytype, _: anytype, writer: anytype) !void {
        const gen = get_generation(self);
        const idx = get_index(self);

        try writer.print("E({x:0>2}:{x:0>6})", .{ gen, idx });
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn init() void {
    gpa = std.heap.GeneralPurposeAllocator(.{}){};
    allocator = gpa.allocator();

    generations = std.ArrayList(EntityGeneration).init(allocator);
    recycled = std.ArrayList(EntityIndex).init(allocator);
    components = std.AutoHashMap(TypeId, ComponentInfo).init(allocator);

    initialized = true;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn deinit() void {
    generations.deinit();
    components.deinit();
    recycled.deinit();

    std.debug.assert(gpa.deinit() == .ok);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn get_index(entity: Entity) EntityIndex {
    const ent: u32 = @intFromEnum(entity);

    const index: EntityIndex = @truncate(ent);
    return index;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn get_generation(entity: Entity) EntityGeneration {
    const ent = @intFromEnum(entity);

    const gen: EntityGeneration = @truncate(ent >> 24);
    return gen;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn mk_entity(gen: EntityGeneration, idx: EntityIndex) Entity {
    const ent: u32 = (@as(u32, @intCast(gen)) << 24) | (@as(u32, @intCast(idx)) & 0xFFFFFF);
    return @enumFromInt(ent);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const ComponentInfo = struct {
    name: []const u8,
    size: usize,
    num_fields: usize,
    fields: [max_component_fields]ComponentField,
    storage: *anyopaque,
};

const max_component_fields = 16;

const ComponentField = struct {
    name: []const u8,
    offset: usize,
    size: usize,
    kind: ComponentFieldKind,
};

const ComponentFieldKind = enum {
    bool,
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    f32,
    f64,
    Entity,
    Vec2,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn register_component(name: []const u8, comptime Component: type) void {
    std.debug.assert(initialized);

    const type_id = get_type_id(Component);
    if (components.get(type_id)) |_| {
        log.debug("component {s} already registered", .{name});
        return;
    }

    log.info("register component {s} {s} {x}", .{ name, @typeName(Component), type_id });

    var info: ComponentInfo = undefined;

    info.name = name;
    info.size = @sizeOf(Component);

    inline for (0.., std.meta.fields(Component)) |i, field| {
        const kind: ComponentFieldKind = switch (field.type) {
            bool => .bool,
            u8 => .u8,
            u16 => .u16,
            u32 => .u32,
            u64 => .u64,
            i8 => .i8,
            i16 => .i16,
            i32 => .i32,
            i64 => .i64,
            f32 => .f32,
            f64 => .f64,
            ng.Vec2 => .Vec2,
            Entity => .Entity,
            else => @compileError(
                @typeName(Component) ++ " field '" ++ field.name ++ "' has an unsupported type for component info : " ++ @typeName(field.type),
            ),
        };
        info.fields[i].name = field.name;
        info.fields[i].offset = @offsetOf(Component, field.name);
        info.fields[i].size = @sizeOf(field.type);
        info.fields[i].kind = kind;
        info.num_fields = i + 1;
    }

    components.put(type_id, info) catch |err| {
        log.err("register_component failed {}", .{err});
        return;
    };
}

const TypeId = usize;

fn get_type_id(comptime Component: type) TypeId {
    return @intFromPtr(&(struct {
        var item: u8 = @intCast(@sizeOf(Component));
    }).item);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn new() Entity {
    std.debug.assert(initialized);

    if (recycled.pop()) |idx| {
        const gen = generations.items[idx];
        return mk_entity(gen, idx);
    }

    const index: EntityIndex = @intCast(generations.items.len);
    generations.append(0) catch |err| {
        log.err("new {}", .{err});
        return .null_entity;
    };

    return mk_entity(0, index);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn dump_ecs() void {
    std.debug.assert(initialized);
    log.msg("Dump ECS", .{});

    var component_iter = components.iterator();
    while (component_iter.next()) |entry| {
        const component = entry.value_ptr;
        log.msg("  Component {s} ({} bytes)", .{ component.name, component.size });
        for (0..component.num_fields) |i| {
            const field = component.fields[i];
            log.msg("    {s}: {s} (offset {}, {} bytes)", .{
                field.name,
                @tagName(field.kind),
                field.offset,
                field.size,
            });
        }
    }

    for (0.., generations.items) |idx, gen| {
        const ent = mk_entity(gen, @intCast(idx));
        log.msg("  Entity {}", .{ent});
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
