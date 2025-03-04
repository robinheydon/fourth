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

var generations: std.ArrayListUnmanaged(EntityGeneration) = .empty;
var recycled: std.ArrayListUnmanaged(EntityIndex) = .empty;
var old_recycled: std.ArrayListUnmanaged(EntityIndex) = .empty;

var components: std.AutoHashMapUnmanaged(TypeId, ComponentTypeInfo) = .empty;

var systems: std.ArrayListUnmanaged(SystemInfo) = .empty;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const EntityIndex = u24;
const EntityGeneration = u8;

pub const Entity = enum(u32) {
    null_entity = 0xFFFFFFFF,
    _,

    ///////////////////////////////////////////////////////////////////////////////////////////

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

    ///////////////////////////////////////////////////////////////////////////////////////////

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

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn delete(self: Entity) void {
        if (!self.is_valid()) {
            log.err("delete invalid entity {}", .{self});
            return;
        }

        const gen = get_generation(self);
        const idx = get_index(self);

        generations.items[idx] = gen +% 1;
        if (generations.items[idx] == 255) {
            old_recycled.append(allocator, idx) catch |err| {
                log.err("delete entity failed {}", .{err});
                return;
            };
        } else {
            recycled.append(allocator, idx) catch |err| {
                log.err("delete entity failed {}", .{err});
                return;
            };
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

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

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn format(self: Entity, _: anytype, _: anytype, writer: anytype) !void {
        const gen = get_generation(self);
        const idx = get_index(self);

        try writer.print("E({x:0>2}:{x:0>6})", .{ gen, idx });
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn init(external_allocator: ?std.mem.Allocator) void {
    gpa = std.heap.GeneralPurposeAllocator(.{}){};
    if (external_allocator) |alloc| {
        allocator = alloc;
    } else {
        allocator = gpa.allocator();
    }

    generations = .empty;
    recycled = .empty;
    old_recycled = .empty;
    components = .empty;
    systems = .empty;

    initialized = true;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn deinit() void {
    var component_iter = components.iterator();
    while (component_iter.next()) |entry| {
        const component = entry.value_ptr;
        component.storage.deinit(component.storage);
    }

    generations.deinit(allocator);
    recycled.deinit(allocator);
    old_recycled.deinit(allocator);
    components.deinit(allocator);
    systems.deinit(allocator);

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

pub fn new() Entity {
    std.debug.assert(initialized);

    if (recycled.pop()) |idx| {
        const gen = generations.items[idx];
        return mk_entity(gen, idx);
    }

    const index: EntityIndex = @intCast(generations.items.len);
    generations.append(allocator, 0) catch |err| {
        log.err("new {}", .{err});
        return .null_entity;
    };

    return mk_entity(0, index);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn recycle_old_generations() void {
    while (old_recycled.pop()) |idx| {
        const gen = generations.items[idx];
        generations.items[idx] = gen +% 1;
        recycled.append(allocator, idx) catch |err|
            {
                log.err("recycle_old_generations {}", .{err});
            };
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const max_component_fields = 16;
const max_enum_values = 32;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const ComponentTypeInfo = struct {
    storage: ErasedComponentStorage = undefined,
    name: []const u8 = undefined,
    size: usize = undefined,
    info: ComponentData = undefined,

    pub fn get_data(self: ComponentTypeInfo, ent: Entity) ?[]const u8 {
        return self.storage.get_data(self.storage, ent);
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const ComponentData = union(ComponentKind) {
    @"struct": ComponentStructInfo,
    @"enum": ComponentEnumInfo,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const ComponentKind = enum {
    @"struct",
    @"enum",
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const ComponentStructInfo = struct {
    num_fields: usize,
    fields: [max_component_fields]ComponentField,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const ComponentEnumInfo = struct {
    tag_type: ComponentEnumKind,
    num_values: usize,
    values: [max_enum_values]EnumValue,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const ComponentField = struct {
    name: []const u8,
    offset: usize,
    size: usize,
    kind: ComponentFieldKind,
    component: usize,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const EnumValue = struct {
    name: []const u8,
    value: usize,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

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
    Component,
    unknown,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const ComponentEnumKind = enum {
    u8,
    u16,
    u32,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn ComponentStorage(Component: type) type {
    return struct {
        store: std.AutoArrayHashMapUnmanaged(EntityIndex, Component) = .empty,

        const Self = @This();

        pub const empty: Self = .{
            .store = .empty,
        };

        pub fn deinit(self: *Self) void {
            self.store.deinit(allocator);
        }

        pub fn set(self: *Self, key: Entity, value: anytype) void {
            const idx = get_index(key);
            self.store.put(allocator, idx, value) catch |err| {
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

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

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

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn init_erased_component_storage(Component: type) ErasedComponentStorage {
    const ptr = allocator.create(ComponentStorage(Component)) catch unreachable;
    ptr.* = .empty;

    return ErasedComponentStorage{
        .ptr = ptr,
        .deinit = (struct {
            fn deinit(self: ErasedComponentStorage) void {
                const cast_ptr = self.cast(Component);
                cast_ptr.deinit();
                allocator.destroy(cast_ptr);
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

    const component_typeinfo = @typeInfo(Component);

    switch (component_typeinfo) {
        .@"struct" => {
            var info: ComponentStructInfo = undefined;
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
                    else => blk: {
                        const field_type_id = get_type_id(field.type);
                        if (components.get(field_type_id)) |field_com| {
                            _ = field_com;
                            info.fields[i].component = field_type_id;
                            break :blk .Component;
                        } else {
                            log.fatal("{s} field {s} has an unsupported type : {s}", .{
                                @typeName(Component),
                                field.name,
                                @typeName(field.type),
                            });
                        }
                    },
                };
                info.fields[i].name = field.name;
                info.fields[i].offset = @offsetOf(Component, field.name);
                info.fields[i].size = @sizeOf(field.type);
                info.fields[i].kind = kind;
                info.num_fields = i + 1;
            }

            const type_info: ComponentTypeInfo = .{
                .name = name,
                .size = @sizeOf(Component),
                .info = .{ .@"struct" = info },
                .storage = init_erased_component_storage(Component),
            };

            components.put(allocator, type_id, type_info) catch |err| {
                log.err("register_component failed {}", .{err});
                return;
            };
        },
        .@"enum" => |zig_enum| {
            var info: ComponentEnumInfo = undefined;

            info.tag_type = switch (zig_enum.tag_type) {
                u8 => .u8,
                u16 => .u16,
                u32 => .u32,
                else => {
                    @compileError(
                        @typeName(Component) ++
                            " tag_type has an unsupported type for enum : " ++
                            @typeName(zig_enum.tag_type),
                    );
                },
            };

            inline for (0.., std.meta.fields(Component)) |i, field| {
                info.values[i].name = field.name;
                info.values[i].value = field.value;
                info.num_values = i + 1;
            }

            const type_info: ComponentTypeInfo = .{
                .name = name,
                .size = @sizeOf(Component),
                .info = .{ .@"enum" = info },
                .storage = init_erased_component_storage(Component),
            };

            components.put(allocator, type_id, type_info) catch |err| {
                log.err("register_component failed {}", .{err});
                return;
            };
        },
        else => {
            @compileError(@typeName(Component) ++
                " is not a value type. : " ++
                @tagName(component_typeinfo));
        },
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const TypeId = usize;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn get_type_id(comptime Component: type) TypeId {
    return @intFromPtr(&(struct {
        var item: u8 = @intCast(@sizeOf(Component));
    }).item);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const SystemInfo = struct {
    name: []const u8,
    func: *const fn (*const SystemIterator) void,
    phase: SystemPhase,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const SystemIterator = struct {
    delta_time: f32,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const SystemOptions = struct {
    name: []const u8,
    phase: SystemPhase = .update,
};

pub const SystemPhase = enum {
    pre_run,
    run,
    post_run,
    pre_update,
    update,
    post_update,
    pre_store,
    store,
    post_store,
    pre_render,
    render,
    post_render,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn register_system(
    options: SystemOptions,
    func: *const fn (*const SystemIterator) void,
    expression: anytype,
) void {
    const info: SystemInfo = .{
        .name = options.name,
        .func = func,
        .phase = options.phase,
    };

    log.info("register system {s} {x}", .{ options.name, func });

    _ = expression;

    systems.append(allocator, info) catch |err| {
        log.err("register_system failed {}", .{err});
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn progress(dt: f32) void {
    log.info("------------------------", .{});
    log.info("progress ({d:0.3})", .{dt});

    std.sort.block(SystemInfo, systems.items, {}, system_sorter);

    for (systems.items) |system| {
        const iterator = SystemIterator{
            .delta_time = dt,
        };

        system.func(&iterator);
    }
}

fn system_sorter(_: void, a: SystemInfo, b: SystemInfo) bool {
    return @intFromEnum(a.phase) < @intFromEnum(b.phase);
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
        log.msg("Components", .{});
        var component_iter = components.iterator();
        while (component_iter.next()) |entry| {
            const component = entry.value_ptr;
            log.msg("  {s} {s} ({} bytes)", .{
                @tagName(component.info),
                component.name,
                component.size,
            });
            switch (component.info) {
                .@"struct" => |info| {
                    for (0..info.num_fields) |i| {
                        const field = info.fields[i];
                        switch (field.kind) {
                            .Component => {
                                const opt_child = components.get(field.component);
                                if (opt_child) |child| {
                                    log.msg("    {s}: {s} (offset {}, {} bytes)", .{
                                        field.name,
                                        child.name,
                                        field.offset,
                                        field.size,
                                    });
                                }
                            },
                            else => {
                                log.msg("    {s}: {s} (offset {}, {} bytes)", .{
                                    field.name,
                                    @tagName(field.kind),
                                    field.offset,
                                    field.size,
                                });
                            },
                        }
                    }
                },
                .@"enum" => |info| {
                    for (0..info.num_values) |i| {
                        const value = info.values[i];
                        log.msg("    {s} = {}", .{
                            value.name,
                            value.value,
                        });
                    }
                },
            }
        }
    }

    if (show_entities) {
        log.msg("Entities", .{});
        for (0.., generations.items) |idx, gen| {
            const ent = mk_entity(gen, @intCast(idx));
            log.msg("  Entity {}", .{ent});
            if (show_entity_data) {
                var component_iter = components.iterator();
                while (component_iter.next()) |entry| {
                    const component = entry.value_ptr;
                    dump_entity_component("", ent, component);
                }
            }
        }
    }
}

fn dump_entity_component(
    prefix: []const u8,
    ent: Entity,
    component: *const ComponentTypeInfo,
) void {
    switch (component.info) {
        .@"struct" => |info| {
            if (component.get_data(ent)) |data| {
                for (0..info.num_fields) |i| {
                    const field = info.fields[i];
                    switch (field.kind) {
                        .bool => {
                            const field_data =
                                data[field.offset .. field.offset + field.size];
                            const value: *const bool =
                                @alignCast(@ptrCast(&field_data[0]));
                            log.msg("    {s}{s}.{s} = {}", .{
                                prefix,
                                component.name,
                                field.name,
                                value.*,
                            });
                        },
                        .u8 => {
                            const field_data =
                                data[field.offset .. field.offset + field.size];
                            const value: *const u8 =
                                @alignCast(@ptrCast(&field_data[0]));
                            log.msg("    {s}{s}.{s} = {}", .{
                                prefix,
                                component.name,
                                field.name,
                                value.*,
                            });
                        },
                        .u16 => {
                            const field_data =
                                data[field.offset .. field.offset + field.size];
                            const value: *const u16 =
                                @alignCast(@ptrCast(&field_data[0]));
                            log.msg("    {s}{s}.{s} = {}", .{
                                prefix,
                                component.name,
                                field.name,
                                value.*,
                            });
                        },
                        .u32 => {
                            const field_data =
                                data[field.offset .. field.offset + field.size];
                            const value: *const u32 =
                                @alignCast(@ptrCast(&field_data[0]));
                            log.msg("    {s}{s}.{s} = {}", .{
                                prefix,
                                component.name,
                                field.name,
                                value.*,
                            });
                        },
                        .u64 => {
                            const field_data =
                                data[field.offset .. field.offset + field.size];
                            const value: *const u64 =
                                @alignCast(@ptrCast(&field_data[0]));
                            log.msg("    {s}{s}.{s} = {}", .{
                                prefix,
                                component.name,
                                field.name,
                                value.*,
                            });
                        },
                        .i8 => {
                            const field_data =
                                data[field.offset .. field.offset + field.size];
                            const value: *const i8 =
                                @alignCast(@ptrCast(&field_data[0]));
                            log.msg("    {s}{s}.{s} = {}", .{
                                prefix,
                                component.name,
                                field.name,
                                value.*,
                            });
                        },
                        .i16 => {
                            const field_data =
                                data[field.offset .. field.offset + field.size];
                            const value: *const i16 =
                                @alignCast(@ptrCast(&field_data[0]));
                            log.msg("    {s}{s}.{s} = {}", .{
                                prefix,
                                component.name,
                                field.name,
                                value.*,
                            });
                        },
                        .i32 => {
                            const field_data =
                                data[field.offset .. field.offset + field.size];
                            const value: *const i32 =
                                @alignCast(@ptrCast(&field_data[0]));
                            log.msg("    {s}{s}.{s} = {}", .{
                                prefix,
                                component.name,
                                field.name,
                                value.*,
                            });
                        },
                        .i64 => {
                            const field_data =
                                data[field.offset .. field.offset + field.size];
                            const value: *const i64 =
                                @alignCast(@ptrCast(&field_data[0]));
                            log.msg("    {s}{s}.{s} = {}", .{
                                prefix,
                                component.name,
                                field.name,
                                value.*,
                            });
                        },
                        .f32 => {
                            const field_data =
                                data[field.offset .. field.offset + field.size];
                            const value: *const f32 =
                                @alignCast(@ptrCast(&field_data[0]));
                            log.msg("    {s}{s}.{s} = {d:.5}", .{
                                prefix,
                                component.name,
                                field.name,
                                value.*,
                            });
                        },
                        .f64 => {
                            const field_data =
                                data[field.offset .. field.offset + field.size];
                            const value: *const f64 =
                                @alignCast(@ptrCast(&field_data[0]));
                            log.msg("    {s}{s}.{s} = {d:.8}", .{
                                prefix,
                                component.name,
                                field.name,
                                value.*,
                            });
                        },
                        .Vec2 => {
                            const field_data =
                                data[field.offset .. field.offset + field.size];
                            const value: *const ng.Vec2 =
                                @alignCast(@ptrCast(&field_data[0]));
                            log.msg("    {s}{s}.{s} = {d:.5}", .{
                                prefix,
                                component.name,
                                field.name,
                                value.*,
                            });
                        },
                        .Entity => {
                            const field_data =
                                data[field.offset .. field.offset + field.size];
                            const value: *const Entity =
                                @alignCast(@ptrCast(&field_data[0]));
                            log.msg("    {s}{s}.{s} = {}", .{
                                prefix,
                                component.name,
                                field.name,
                                value.*,
                            });
                        },
                        .Component => {
                            log.msg("    {s}{s}.{s} = ?", .{
                                prefix,
                                component.name,
                                field.name,
                            });
                        },
                        .unknown => {
                            log.msg("    {s}{s}.{s} = ??", .{
                                prefix,
                                component.name,
                                field.name,
                            });
                        },
                    }
                }
            }
        },
        .@"enum" => |info| {
            if (component.get_data(ent)) |data| {
                switch (info.tag_type) {
                    .u8 => {
                        const field_data =
                            data[0..1];
                        const value_ptr: *const u8 =
                            @alignCast(@ptrCast(&field_data[0]));
                        const value = value_ptr.*;
                        for (0..info.num_values) |i| {
                            if (info.values[i].value == value) {
                                log.msg("    {s}{s} = .{s}", .{
                                    prefix,
                                    component.name,
                                    info.values[i].name,
                                });
                                break;
                            }
                        } else {
                            log.msg("    {s}{s} = {}", .{
                                prefix,
                                component.name,
                                value,
                            });
                        }
                    },
                    .u16 => {
                        const field_data =
                            data[0..2];
                        const value_ptr: *const u16 =
                            @alignCast(@ptrCast(&field_data[0]));
                        const value = value_ptr.*;
                        for (0..info.num_values) |i| {
                            if (info.values[i].value == value) {
                                log.msg("    {s}{s} = .{s}", .{
                                    prefix,
                                    component.name,
                                    info.values[i].name,
                                });
                                break;
                            }
                        } else {
                            log.msg("    {s}{s} = {}", .{
                                prefix,
                                component.name,
                                value,
                            });
                        }
                    },
                    .u32 => {},
                }
            }
        },
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "init / deinit" {
    init(std.testing.allocator);
    defer deinit();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "create" {
    init(std.testing.allocator);
    defer deinit();

    const e0 = new();
    const e1 = new();
    const e2 = new();
    const e3 = new();

    try std.testing.expectEqual(mk_entity(0, 0), e0);
    try std.testing.expectEqual(mk_entity(0, 1), e1);
    try std.testing.expectEqual(mk_entity(0, 2), e2);
    try std.testing.expectEqual(mk_entity(0, 3), e3);

    try std.testing.expectEqual(true, e0.is_valid());
    try std.testing.expectEqual(true, e1.is_valid());
    try std.testing.expectEqual(true, e2.is_valid());
    try std.testing.expectEqual(true, e3.is_valid());
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "delete" {
    init(std.testing.allocator);
    defer deinit();

    const e0 = new();
    const e1 = new();
    const e2 = new();
    const e3 = new();

    e0.delete();
    e1.delete();
    e2.delete();
    e3.delete();

    try std.testing.expectEqual(mk_entity(0, 0), e0);
    try std.testing.expectEqual(mk_entity(0, 1), e1);
    try std.testing.expectEqual(mk_entity(0, 2), e2);
    try std.testing.expectEqual(mk_entity(0, 3), e3);

    try std.testing.expectEqual(false, e0.is_valid());
    try std.testing.expectEqual(false, e1.is_valid());
    try std.testing.expectEqual(false, e2.is_valid());
    try std.testing.expectEqual(false, e3.is_valid());
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "recycle" {
    init(std.testing.allocator);
    defer deinit();

    const e0 = new();
    const e1 = new();
    const e2 = new();
    const e3 = new();

    e0.delete();
    e1.delete();
    e2.delete();
    e3.delete();

    const e4 = new();
    const e5 = new();
    const e6 = new();
    const e7 = new();
    const e8 = new();

    try std.testing.expectEqual(mk_entity(0, 0), e0);
    try std.testing.expectEqual(mk_entity(0, 1), e1);
    try std.testing.expectEqual(mk_entity(0, 2), e2);
    try std.testing.expectEqual(mk_entity(0, 3), e3);
    try std.testing.expectEqual(mk_entity(1, 3), e4);
    try std.testing.expectEqual(mk_entity(1, 2), e5);
    try std.testing.expectEqual(mk_entity(1, 1), e6);
    try std.testing.expectEqual(mk_entity(1, 0), e7);
    try std.testing.expectEqual(mk_entity(0, 4), e8);
    try std.testing.expectEqual(false, e0.is_valid());
    try std.testing.expectEqual(false, e1.is_valid());
    try std.testing.expectEqual(false, e2.is_valid());
    try std.testing.expectEqual(false, e3.is_valid());
    try std.testing.expectEqual(true, e4.is_valid());
    try std.testing.expectEqual(true, e5.is_valid());
    try std.testing.expectEqual(true, e6.is_valid());
    try std.testing.expectEqual(true, e7.is_valid());
    try std.testing.expectEqual(true, e8.is_valid());
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "generation wrap" {
    init(std.testing.allocator);
    defer deinit();

    var e0 = new();
    e0.delete();

    try std.testing.expectEqual(mk_entity(0, 0), e0);
    try std.testing.expectEqual(false, e0.is_valid());

    for (0..254) |_| {
        e0 = new();
        e0.delete();
    }

    e0 = new();
    try std.testing.expectEqual(mk_entity(0, 1), e0);
    try std.testing.expectEqual(true, e0.is_valid());

    for (0..254) |i| {
        try std.testing.expectEqual(false, mk_entity(@intCast(i), 0).is_valid());
    }

    recycle_old_generations();

    const e1 = new();

    try std.testing.expectEqual(mk_entity(0, 0), e1);
    try std.testing.expectEqual(true, e1.is_valid());
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "set values" {
    init(std.testing.allocator);
    defer deinit();

    const Position = struct {
        x: f32,
        y: f32,
    };

    const Velocity = struct {
        dx: f32,
        dy: f32,
    };

    const Health = struct {
        hp: u8,
    };

    const e0 = new();
    const e1 = new();
    const e2 = new();
    const e3 = new();

    register_component("Position", Position);
    register_component("Velocity", Velocity);
    register_component("Health", Health);

    e0.set(Position{ .x = 2, .y = 3 });
    e0.set(Velocity{ .dx = 0.1, .dy = -0.1 });
    e0.set(Health{ .hp = 20 });

    e1.set(Position{ .x = 4, .y = 5 });
    e1.set(Velocity{ .dx = 0.2, .dy = -0.2 });

    e2.set(Position{ .x = 7, .y = 6 });

    e3.set(Position{ .x = 8, .y = 9 });
    e3.set(Health{ .hp = 10 });

    {
        const pos = e0.get(Position);
        const vel = e0.get(Velocity);
        const hp = e0.get(Health);

        try std.testing.expectEqual(Position{ .x = 2, .y = 3 }, pos);
        try std.testing.expectEqual(Velocity{ .dx = 0.1, .dy = -0.1 }, vel);
        try std.testing.expectEqual(Health{ .hp = 20 }, hp);
    }

    {
        const pos = e1.get(Position);
        const vel = e1.get(Velocity);
        const hp = e1.get(Health);

        try std.testing.expectEqual(Position{ .x = 4, .y = 5 }, pos);
        try std.testing.expectEqual(Velocity{ .dx = 0.2, .dy = -0.2 }, vel);
        try std.testing.expectEqual(null, hp);
    }

    {
        const pos = e2.get(Position);
        const vel = e2.get(Velocity);
        const hp = e2.get(Health);

        try std.testing.expectEqual(Position{ .x = 7, .y = 6 }, pos);
        try std.testing.expectEqual(null, vel);
        try std.testing.expectEqual(null, hp);
    }

    {
        const pos = e3.get(Position);
        const vel = e3.get(Velocity);
        const hp = e3.get(Health);

        try std.testing.expectEqual(Position{ .x = 8, .y = 9 }, pos);
        try std.testing.expectEqual(null, vel);
        try std.testing.expectEqual(Health{ .hp = 10 }, hp);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

test "movement system" {
    init(std.testing.allocator);
    defer deinit();

    const Position = struct {
        x: f32,
        y: f32,
    };

    const Velocity = struct {
        dx: f32,
        dy: f32,
    };

    register_component("Position", Position);
    register_component("Velocity", Velocity);

    const movement_system = (struct {
        fn movement_system(pos: []Position) void {
            _ = pos;
        }
    }).movement_system;

    register_system(.{ .name = "Movement System" }, movement_system, .{ Position, Velocity });

    const e0 = new();
    const e1 = new();

    e0.set(Position{ .x = 2, .y = 3 });
    e0.set(Velocity{ .dx = 0.2, .dy = -0.1 });

    e1.set(Position{ .x = 4, .y = 5 });
    e1.set(Velocity{ .dx = 0.1, .dy = -0.2 });

    progress(1);
    progress(1);

    const p0 = e0.get(Position);
    const p1 = e0.get(Position);

    try std.testing.expectEqual(Position{ .x = 2.4, .y = 2.8 }, p0);
    try std.testing.expectEqual(Position{ .x = 4.2, .y = 4.6 }, p1);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
