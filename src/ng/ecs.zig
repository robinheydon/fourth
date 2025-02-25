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
var ecs_allocator: std.mem.Allocator = undefined;

var initialized = false;

var next_entity_index: usize = 0;

var generations: std.ArrayListUnmanaged(EntityGeneration) = .empty;
var recycled: std.ArrayListUnmanaged(EntityIndex) = .empty;

var components: std.AutoHashMapUnmanaged(TypeId, ComponentInfo) = .empty;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const EntityIndex = u24;
const EntityGeneration = u8;

pub const Entity = enum(u32) {
    null_entity = 0xFFFFFFFF,
    _,

    pub fn set(self: Entity, value: anytype) void {
        const Component = @TypeOf(value);

        if (!self.is_valid()) {
            log.err("set invalid entity {}", .{self});
            return;
        }

        const typeid = get_type_id(Component);

        if (components.getPtr(typeid)) |info| {
            var storage = info.storage.cast(Component);
            storage.set(self, value);
        } else {
            log.err("Component {s} not registered", .{@typeName(Component)});
        }
    }

    pub fn get(self: Entity, comptime Component: type) ?Component {
        if (!self.is_valid()) {
            log.err("get invalid entity {}", .{self});
            return null;
        }

        const typeid = get_type_id(Component);

        if (components.getPtr(typeid)) |info| {
            var storage = info.storage.cast(Component);
            return storage.get(self);
        } else {
            log.err("Component {s} not registered", .{@typeName(Component)});
        }
        return null;
    }

    pub fn delete(self: Entity) void {
        if (!self.is_valid()) {
            log.err("delete invalid entity {}", .{self});
            return;
        }

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
    ecs_allocator = gpa.allocator();

    generations = .empty;
    recycled = .empty;
    components = .empty;

    initialized = true;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn deinit() void {
    generations.deinit(ecs_allocator);
    var component_iter = components.iterator();
    while (component_iter.next()) |entry| {
        const component = entry.value_ptr;
        component.storage.deinit(component.storage);
    }
    components.deinit(ecs_allocator);
    recycled.deinit(ecs_allocator);

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
    storage: ErasedComponentStorage,
    name: []const u8,
    size: usize,
    num_fields: usize,
    fields: [max_component_fields]ComponentField,

    pub fn get_data(self: ComponentInfo, ent: Entity) ?[]const u8 {
        return self.storage.get_data(self.storage, ent);
    }
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

pub fn ComponentStorage(Component: type) type {
    return struct {
        store: std.AutoArrayHashMapUnmanaged(EntityIndex, Component) = .empty,

        const Self = @This();

        pub const empty: Self = .{
            .store = .empty,
        };

        pub fn deinit(self: *Self) void {
            self.store.deinit(ecs_allocator);
        }

        pub fn set(self: *Self, key: Entity, value: anytype) void {
            const idx = get_index(key);
            self.store.put(ecs_allocator, idx, value) catch |err| {
                log.err("Cannot set component {} {} : {}", .{ key, value, err });
            };
        }

        pub fn get(self: *Self, key: Entity) ?Component {
            const idx = get_index(key);
            return self.store.get(idx);
        }

        pub fn get_data(self: *Self, key: Entity) ?[]const u8 {
            const idx = get_index(key);
            if (self.store.getPtr(idx)) |value| {
                var ptr: []const u8 = undefined;
                ptr.ptr = @ptrCast(value);
                ptr.len = @sizeOf(Component);
                return ptr;
            }
            return null;
        }
    };
}

pub const ErasedComponentStorage = struct {
    ptr: *anyopaque,
    deinit: *const fn (self: ErasedComponentStorage) void,
    get_data: *const fn (self: ErasedComponentStorage, ent: Entity) ?[]const u8,

    pub fn cast(
        self: ErasedComponentStorage,
        Component: type,
    ) *ComponentStorage(Component) {
        return @alignCast(@ptrCast(self.ptr));
    }
};

fn init_erased_component_storage(Component: type) ErasedComponentStorage {
    const ptr = ecs_allocator.create(ComponentStorage(Component)) catch unreachable;
    ptr.* = .empty;

    return ErasedComponentStorage{
        .ptr = ptr,
        .deinit = (struct {
            fn deinit(self: ErasedComponentStorage) void {
                const cast_ptr = self.cast(Component);
                cast_ptr.deinit();
                ecs_allocator.destroy(cast_ptr);
            }
        }).deinit,
        .get_data = (struct {
            fn get_data(self: ErasedComponentStorage, ent: Entity) ?[]const u8 {
                const cast_ptr = self.cast(Component);
                return cast_ptr.get_data(ent);
            }
        }).get_data,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn register_component(comptime name: []const u8, comptime Component: type) void {
    std.debug.assert(initialized);

    const type_id = get_type_id(Component);
    if (components.get(type_id)) |_| {
        log.warn("component {s} already registered", .{name});
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

    info.storage = init_erased_component_storage(Component);

    components.put(ecs_allocator, type_id, info) catch |err| {
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
    generations.append(ecs_allocator, 0) catch |err| {
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

    const show_components = true;
    const show_entities = true;
    const show_entity_data = true;

    if (show_components) {
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
    }

    if (show_entities) {
        for (0.., generations.items) |idx, gen| {
            const ent = mk_entity(gen, @intCast(idx));
            log.msg("  Entity {}", .{ent});
            if (show_entity_data) {
                var component_iter = components.iterator();
                while (component_iter.next()) |entry| {
                    const component = entry.value_ptr;
                    if (component.get_data(ent)) |data| {
                        for (0..component.num_fields) |i| {
                            const field = component.fields[i];
                            const field_data = data[field.offset .. field.offset + field.size];
                            switch (field.kind) {
                                .u8 => {
                                    const value: *const u8 = @alignCast(@ptrCast(&field_data[0]));
                                    log.msg("    {s}.{s}:u8 = {any}", .{
                                        component.name,
                                        field.name,
                                        value.*,
                                    });
                                },
                                .u16 => {
                                    const value: *const u16 = @alignCast(@ptrCast(&field_data[0]));
                                    log.msg("    {s}.{s}:u16 = {any}", .{
                                        component.name,
                                        field.name,
                                        value.*,
                                    });
                                },
                                .f32 => {
                                    const value: *const f32 = @alignCast(@ptrCast(&field_data[0]));
                                    log.msg("    {s}.{s}:f32 = {d}", .{
                                        component.name,
                                        field.name,
                                        value.*,
                                    });
                                },
                                .Vec2 => {
                                    const value: *const ng.Vec2 = @alignCast(@ptrCast(&field_data[0]));
                                    log.msg("    {s}.{s}:Vec2 = {d}", .{
                                        component.name,
                                        field.name,
                                        value.*,
                                    });
                                },
                                .Entity => {
                                    const value: *const Entity = @alignCast(@ptrCast(&field_data[0]));
                                    log.msg("    {s}.{s}:Entity = {d}", .{
                                        component.name,
                                        field.name,
                                        value.*,
                                    });
                                },
                                else => {
                                    log.msg("    {s}.{s}:{s} = {any}", .{
                                        component.name,
                                        field.name,
                                        @tagName(field.kind),
                                        field_data,
                                    });
                                },
                            }
                        }
                    }
                }
            }
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
