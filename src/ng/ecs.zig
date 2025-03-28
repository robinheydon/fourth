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

var generations: std.ArrayListUnmanaged(EntityGeneration) = .empty;
var recycled: std.ArrayListUnmanaged(EntityIndex) = .empty;
var old_recycled: std.ArrayListUnmanaged(EntityIndex) = .empty;

var components: std.ArrayListUnmanaged(ComponentInfo) = .empty;

var systems: std.ArrayListUnmanaged(SystemInfo) = .empty;

var queries: std.ArrayListUnmanaged(QueryInfo) = .empty;

var entity_changes: std.ArrayHashMapUnmanaged(
    EntityIndex,
    void,
    EntityContext,
    false,
) = .empty;

var staged: bool = false;
var entity_update_data: std.ArrayListUnmanaged(u8) = .empty;
var entity_updates: std.ArrayListUnmanaged(EntityCommand) = .empty;

var inside_system: bool = false;
var current_system: *const SystemInfo = undefined;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const EntityContext = struct {
    pub fn hash(_: EntityContext, key: EntityIndex) u32 {
        return key;
    }
    pub fn eql(
        _: EntityContext,
        a: EntityIndex,
        b: EntityIndex,
        _: usize,
    ) bool {
        return a == b;
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const EntityCommand = union(EntityCommandKind) {
    set: struct { entity: Entity, type_id: usize, start: usize, len: usize },
    remove: struct { entity: Entity, type_id: usize },
    delete: struct { entity: Entity },
};

pub const EntityCommandKind = enum {
    set,
    remove,
    delete,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const EntityIndex = u24;
const EntityGeneration = u8;

pub const Entity = packed struct(u32) {
    idx: EntityIndex,
    gen: EntityGeneration,

    pub const nil: Entity = .{ .idx = 0xFFFFFF, .gen = 0xFF };

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn set(self: Entity, value: anytype) void {
        const Component = @TypeOf(value);

        if (!self.is_valid()) {
            log.err("set invalid entity {}", .{self});
            return;
        }

        const type_id = get_type_id(Component);

        const info = components.items[type_id];
        if (staged) {
            const bytes = ng.as_bytes(&value);

            const start = entity_update_data.items.len;
            entity_update_data.appendSlice(allocator, bytes) catch {
                log.err("OOM set staged {} {s}", .{ self, info.name });
                return;
            };

            entity_updates.append(allocator, .{ .set = .{
                .entity = self,
                .type_id = type_id,
                .start = start,
                .len = bytes.len,
            } }) catch {
                log.err("OOM set staged {} {s}", .{ self, info.name });
                return;
            };
        } else {
            var storage = info.storage.cast(Component);
            if (storage.getPtr(self)) |_| {
                storage.set(self, value);
            } else {
                entity_changes.put(allocator, self.idx, {}) catch {
                    log.err("OOM set entity changes {} {s}", .{ self, info.name });
                };
                storage.set(self, value);
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn get(self: Entity, comptime Component: type) ?Component {
        if (!self.is_valid()) {
            log.err("get invalid entity {}", .{self});
            return null;
        }

        const type_id = get_type_id(Component);

        const info = components.items[type_id];
        const storage = info.storage.cast(Component);
        return storage.get(self);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn getPtr(self: Entity, comptime Component: type) ?*Component {
        if (!self.is_valid()) {
            log.err("get invalid entity ptr {}", .{self});
            return null;
        }

        const type_id = get_type_id(Component);

        if (inside_system) {
            for (0..current_system.num_arguments) |i| {
                if (type_id == current_system.arguments[i]) {
                    if (!current_system.mutable[i]) {
                        log.err("System {s} called getPtr on a non-mutable component {s}", .{
                            current_system.name,
                            @typeName(Component),
                        });
                        return null;
                    }
                }
            }
        }

        const info = components.items[type_id];
        const storage = info.storage.cast(Component);
        return storage.getPtr(self);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn has(self: Entity, Component: type) bool {
        const type_id = get_type_id(Component);
        return self.has_component_type(type_id);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn has_component_type(self: Entity, type_id: TypeId) bool {
        const info = components.items[type_id];
        return info.storage.contains(info.storage, self);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn remove(self: Entity, Component: type) void {
        if (!self.is_valid()) {
            log.err("remove invalid entity component {} {s}", .{ self, @typeName(Component) });
            return;
        }

        const type_id = get_type_id(Component);

        const info = components.items[type_id];
        if (staged) {
            entity_updates.append(allocator, .{ .remove = .{
                .entity = self,
                .type_id = type_id,
            } }) catch {
                log.err("OOM remove {} {s}", .{ self, info.name });
                return;
            };
        } else {
            var storage = info.storage.cast(Component);
            entity_changes.put(allocator, self.idx, {}) catch {
                log.err("OOM remove {} {s}", .{ self, info.name });
            };
            storage.remove(self);
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn delete(self: Entity) void {
        if (!self.is_valid()) {
            log.err("delete invalid entity {}", .{self});
            return;
        }

        if (staged) {
            entity_updates.append(allocator, .{ .delete = .{
                .entity = self,
            } }) catch {
                log.err("OOM delete {}", .{self});
                return;
            };
        } else {
            entity_changes.put(allocator, self.idx, {}) catch {};

            const gen = self.gen;
            const idx = self.idx;

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

            for (components.items) |component| {
                component.storage.remove(component.storage, self);
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn is_valid(self: Entity) bool {
        if (self.idx >= generations.items.len) {
            return false;
        }
        if (generations.items[self.idx] != self.gen) {
            return false;
        }
        return true;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////

    pub fn format(self: Entity, _: anytype, _: anytype, writer: anytype) !void {
        const gen = self.gen;
        const idx = self.idx;

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
    entity_changes = .empty;
    entity_updates = .empty;
    entity_update_data = .empty;

    initialized = true;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn deinit() void {
    for (components.items) |component| {
        component.type_id_ptr.* = invalid_type_id;
        component.storage.deinit(component.storage);
    }

    for (systems.items) |*sys| {
        sys.entities.deinit(allocator);
    }

    for (queries.items) |*query| {
        query.entities.deinit(allocator);
    }

    generations.deinit(allocator);
    recycled.deinit(allocator);
    old_recycled.deinit(allocator);
    components.deinit(allocator);
    systems.deinit(allocator);
    queries.deinit(allocator);
    entity_changes.deinit(allocator);
    entity_updates.deinit(allocator);
    entity_update_data.deinit(allocator);

    std.debug.assert(gpa.deinit() == .ok);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn new() Entity {
    std.debug.assert(initialized);

    if (recycled.pop()) |idx| {
        const gen = generations.items[idx];
        const self = Entity{ .gen = gen, .idx = idx };
        entity_changes.put(allocator, self.idx, {}) catch {};
        return self;
    }

    const index: EntityIndex = @intCast(generations.items.len);
    generations.append(allocator, 0) catch |err| {
        log.err("new {}", .{err});
        return .nil;
    };

    const self = Entity{ .gen = 0, .idx = index };
    entity_changes.put(allocator, self.idx, {}) catch {};

    return self;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn recycle_old_generations() void {
    while (old_recycled.pop()) |idx| {
        const gen = generations.items[idx];
        generations.items[idx] = gen +% 1;
        recycled.append(allocator, idx) catch |err| {
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

const ComponentInfo = struct {
    storage: ErasedComponentStorage = undefined,
    name: []const u8 = undefined,
    size: usize = undefined,
    info: ComponentData = undefined,
    type_id_ptr: *u32 = undefined,

    pub fn get_data(self: ComponentInfo, ent: Entity) ?[]const u8 {
        return self.storage.get_data(self.storage, ent);
    }

    pub fn contains(self: ComponentInfo, ent: Entity) bool {
        return self.storage.contains(self.storage, ent);
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

const ComponentField = struct {
    name: []const u8,
    offset: usize,
    size: usize,
    kind: ComponentFieldKind,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const ComponentEnumInfo = struct {
    tag_type: ComponentFieldKind,
    num_values: u8,
    values: [max_enum_values]EnumValue,
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
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn ComponentStorage(Component: type) type {
    return struct {
        store: std.ArrayHashMapUnmanaged(
            EntityIndex,
            Component,
            EntityContext,
            false,
        ) = .empty,

        const Self = @This();

        pub const empty: Self = .{ .store = .empty };

        pub fn deinit(self: *Self) void {
            self.store.deinit(allocator);
        }

        pub fn set(self: *Self, key: Entity, value: anytype) void {
            const idx = key.idx;
            self.store.put(allocator, idx, value) catch |err| {
                log.err("Cannot set component {} {} : {}", .{ key, value, err });
            };
        }

        pub fn get(self: *Self, key: Entity) ?Component {
            return self.store.get(key.idx);
        }

        pub fn getPtr(self: *Self, key: Entity) ?*Component {
            const idx = key.idx;
            return self.store.getPtr(idx);
        }

        pub fn remove(self: *Self, key: Entity) void {
            const idx = key.idx;
            _ = self.store.swapRemove(idx);
        }

        pub fn get_data(self: *Self, key: Entity) ?[]const u8 {
            const idx = key.idx;
            if (self.store.getPtr(idx)) |value| {
                var ptr: []const u8 = undefined;
                ptr.ptr = @ptrCast(value);
                ptr.len = @sizeOf(Component);
                return ptr;
            }
            return null;
        }

        pub fn set_data(self: *Self, key: Entity, data: []const u8) void {
            const value: *Component = @constCast(@alignCast(@ptrCast(data)));
            self.store.put(allocator, key.idx, value.*) catch |err| {
                log.err("Cannot set component {} {} : {}", .{ key, value, err });
            };
        }

        pub fn contains(self: *Self, key: Entity) bool {
            return self.store.contains(key.idx);
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
    set_data: *const fn (self: ErasedComponentStorage, ent: Entity, data: []const u8) void,
    remove: *const fn (self: ErasedComponentStorage, ent: Entity) void,
    contains: *const fn (self: ErasedComponentStorage, ent: Entity) bool,

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
        .set_data = (struct {
            fn set_data(self: ErasedComponentStorage, ent: Entity, data: []const u8) void {
                const cast_ptr = self.cast(Component);
                cast_ptr.set_data(ent, data);
            }
        }).set_data,
        .remove = (struct {
            fn remove(self: ErasedComponentStorage, ent: Entity) void {
                const cast_ptr = self.cast(Component);
                cast_ptr.remove(ent);
            }
        }).remove,
        .contains = (struct {
            fn contains(self: ErasedComponentStorage, ent: Entity) bool {
                const cast_ptr = self.cast(Component);
                return cast_ptr.contains(ent);
            }
        }).contains,
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn register_component(comptime name: []const u8, comptime Component: type) void {
    std.debug.assert(initialized);

    const type_id_ptr = get_type_id_ptr(Component);
    if (type_id_ptr.* != invalid_type_id) {
        log.warn("component {s} already registered", .{name});
        return;
    }

    const type_id: TypeId = @intCast(components.items.len);
    type_id_ptr.* = type_id;

    log.info("register component \"{}\" {s} {x}", .{
        std.zig.fmtEscapes(name),
        @typeName(Component),
        type_id,
    });

    const component_typeinfo = @typeInfo(Component);

    switch (component_typeinfo) {
        .@"struct" => {
            var info: ComponentStructInfo = undefined;
            info.num_fields = 0;
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

            const type_info: ComponentInfo = .{
                .name = name,
                .size = @sizeOf(Component),
                .info = .{ .@"struct" = info },
                .storage = init_erased_component_storage(Component),
                .type_id_ptr = type_id_ptr,
            };

            components.append(allocator, type_info) catch |err| {
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

            const type_info: ComponentInfo = .{
                .name = name,
                .size = @sizeOf(Component),
                .info = .{ .@"enum" = info },
                .storage = init_erased_component_storage(Component),
                .type_id_ptr = type_id_ptr,
            };

            components.append(allocator, type_info) catch |err| {
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

const TypeId = u32;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn get_type_id(comptime Component: type) TypeId {
    const ptr = get_type_id_ptr(Component);
    if (ptr.* == invalid_type_id) {
        log.fatal("Invalid Component in get_type_id {s}", .{@typeName(Component)});
    }
    return ptr.*;
}

const invalid_type_id: TypeId = std.math.maxInt(TypeId);

fn get_type_id_ptr(comptime Component: type) *TypeId {
    return &(struct {
        var index: u32 = invalid_type_id;
        var size: u32 = @sizeOf(Component);
    }).index;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const SystemInfo = struct {
    name: []const u8,
    func: *const fn (*const SystemIterator) void,
    phase: SystemPhase,
    last_elapsed: f64 = 0,
    interval: f32 = 0,
    wait_time: f32 = 0,
    num_arguments: usize,
    arguments: [max_system_arguments]TypeId,
    mutable: [max_system_arguments]bool,
    entities: std.ArrayHashMapUnmanaged(Entity, void, SystemEntityContext, false),
};

const SystemEntityContext = struct {
    pub fn hash(_: SystemEntityContext, key: Entity) u32 {
        return @bitCast(key);
    }
    pub fn eql(
        _: SystemEntityContext,
        a: Entity,
        b: Entity,
        _: usize,
    ) bool {
        return a == b;
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const max_system_arguments = 16;

pub const SystemArgument = struct {
    component: TypeId,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const SystemIterator = struct {
    delta_time: f32,
    entities: []Entity,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const SystemOptions = struct {
    name: []const u8,
    phase: SystemPhase = .update,
    interval: f32 = 0,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const SystemPhase = enum {
    first_phase,
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
    render0,
    render1,
    render2,
    render3,
    post_render,
    last_phase,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn register_system(
    options: SystemOptions,
    func: *const fn (*const SystemIterator) void,
    expression: anytype,
) void {
    log.info("register system {s}", .{options.name});

    var num_arguments: usize = 0;
    var arguments: [max_system_arguments]TypeId = undefined;
    var mutable: [max_system_arguments]bool = undefined;

    inline for (expression) |arg| {
        const type_info = @typeInfo(arg);
        switch (type_info) {
            .pointer => |ptr| {
                const child = ptr.child;
                comptime std.debug.assert(ptr.size == .one);
                if (@typeInfo(child) == .@"struct") {
                    arguments[num_arguments] = get_type_id(type_info.pointer.child);
                    mutable[num_arguments] = true;
                    num_arguments += 1;
                } else if (@typeInfo(child) == .@"enum") {
                    arguments[num_arguments] = get_type_id(type_info.pointer.child);
                    mutable[num_arguments] = true;
                    num_arguments += 1;
                } else {
                    @compileError("Invalid type in register_system expression : " ++
                        @typeName(arg));
                }
            },
            .@"struct" => |_| {
                arguments[num_arguments] = get_type_id(arg);
                mutable[num_arguments] = false;
                num_arguments += 1;
            },
            .@"enum" => |_| {
                arguments[num_arguments] = get_type_id(arg);
                mutable[num_arguments] = false;
                num_arguments += 1;
            },
            else => {
                @compileError("Invalid type in register_system expression : " ++
                    @typeName(arg));
            },
        }
    }

    const info: SystemInfo = .{
        .name = options.name,
        .func = func,
        .phase = options.phase,
        .interval = options.interval,
        .wait_time = options.interval,
        .num_arguments = num_arguments,
        .arguments = arguments,
        .mutable = mutable,
        .entities = .empty,
    };

    systems.append(allocator, info) catch |err| {
        log.err("register_system failed {}", .{err});
    };
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn progress(dt: f32) void {
    std.sort.block(SystemInfo, systems.items, {}, system_sorter);

    var number_used_phases: usize = 0;
    var used_phases: [16]SystemPhase = undefined;

    for (systems.items) |system| {
        if (number_used_phases == 0 or used_phases[number_used_phases - 1] != system.phase) {
            used_phases[number_used_phases] = system.phase;
            number_used_phases += 1;
        }
    }

    for (used_phases[0..number_used_phases]) |phase| {
        update_system_entities();

        staged = true;
        for (systems.items) |*system| {
            if (system.phase == phase) {
                var run_system: bool = undefined;
                if (system.interval > 0) {
                    system.wait_time -= dt;
                    if (system.wait_time < 0) {
                        system.wait_time += system.interval;
                        run_system = true;
                    } else {
                        run_system = false;
                    }
                } else {
                    run_system = true;
                }
                if (run_system) {
                    const start_time = ng.elapsed();

                    const iterator = SystemIterator{
                        .delta_time = dt,
                        .entities = system.entities.keys(),
                    };

                    current_system = system;
                    inside_system = true;
                    defer inside_system = false;
                    system.func(&iterator);
                    current_system = undefined;

                    const end_time = ng.elapsed();
                    system.last_elapsed = end_time - start_time;
                }
            }
        }
        staged = false;

        for (entity_updates.items) |command| {
            switch (command) {
                .set => |cmd| {
                    const data = entity_update_data.items[cmd.start .. cmd.start + cmd.len];

                    const info = components.items[cmd.type_id];
                    info.storage.set_data(info.storage, cmd.entity, data);

                    entity_changes.put(allocator, cmd.entity.idx, {}) catch {
                        log.err("OOM set entity changes {}", .{cmd.entity});
                    };
                },
                .remove => |cmd| {
                    const info = components.items[cmd.type_id];
                    info.storage.remove(info.storage, cmd.entity);

                    entity_changes.put(allocator, cmd.entity.idx, {}) catch {
                        log.err("OOM remove entity changes {}", .{cmd.entity});
                    };
                },
                .delete => |cmd| {
                    cmd.entity.delete();

                    entity_changes.put(allocator, cmd.entity.idx, {}) catch {
                        log.err("OOM delete entity changes {}", .{cmd.entity});
                    };
                },
            }
        }

        entity_updates.clearRetainingCapacity();
        entity_update_data.clearRetainingCapacity();
    }

    update_system_entities();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn system_sorter(_: void, a: SystemInfo, b: SystemInfo) bool {
    if (a.phase == b.phase) {
        return a.last_elapsed > b.last_elapsed;
    }
    return @intFromEnum(a.phase) < @intFromEnum(b.phase);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn update_system_entities() void {
    const keys = entity_changes.keys();
    for (keys) |idx| {
        const entity = Entity{ .idx = idx, .gen = generations.items[idx] };
        for (systems.items) |*sys| {
            var matches = true;
            for (sys.arguments[0..sys.num_arguments]) |arg| {
                if (!entity.has_component_type(arg)) {
                    matches = false;
                    // break;
                }
            }
            if (matches) {
                sys.entities.put(allocator, entity, {}) catch {};
            } else {
                _ = sys.entities.swapRemove(entity);
            }
        }
        for (queries.items) |*query| {
            var matches = true;
            for (query.arguments[0..query.num_arguments]) |arg| {
                if (!entity.has_component_type(arg)) {
                    matches = false;
                    // break;
                }
            }
            if (matches) {
                query.entities.put(allocator, entity, {}) catch {};
            } else {
                _ = query.entities.swapRemove(entity);
            }
        }
    }
    entity_changes.clearRetainingCapacity();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const QueryInfo = struct {
    name: []const u8,
    num_arguments: usize,
    arguments: [max_system_arguments]TypeId,
    entities: std.ArrayHashMapUnmanaged(Entity, void, SystemEntityContext, false),
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const QueryIterator = struct {
    index: usize,
    entities: []Entity,

    pub fn next(self: *QueryIterator) ?Entity {
        if (self.index < self.entities.len) {
            const entity = self.entities[self.index];
            self.index += 1;
            return entity;
        }
        return null;
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Query = enum(u32) {
    _,
    pub fn iterator(self: Query) QueryIterator {
        const index = @intFromEnum(self);
        const query = queries.items[index];
        return .{
            .index = 0,
            .entities = query.entities.keys(),
        };
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const QueryOptions = struct {
    name: []const u8,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn register_query(options: QueryOptions, expression: anytype) Query {
    var num_arguments: usize = 0;
    var arguments: [max_system_arguments]TypeId = undefined;
    var mutable: [max_system_arguments]bool = undefined;

    inline for (expression) |arg| {
        const type_info = @typeInfo(arg);
        switch (type_info) {
            .@"struct" => |_| {
                arguments[num_arguments] = get_type_id(arg);
                mutable[num_arguments] = false;
                num_arguments += 1;
            },
            .@"enum" => |_| {
                arguments[num_arguments] = get_type_id(arg);
                mutable[num_arguments] = false;
                num_arguments += 1;
            },
            else => {
                @compileError("Invalid type in register_system expression : " ++
                    @typeName(arg));
            },
        }
    }

    const info: QueryInfo = .{
        .name = options.name,
        .num_arguments = num_arguments,
        .arguments = arguments,
        .entities = .empty,
    };

    const index = queries.items.len;

    queries.append(allocator, info) catch |err| {
        log.err("register_system failed {}", .{err});
    };
    return @enumFromInt(index);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const SaveMemory = struct {
    memory: std.ArrayList(u8),
    strings: std.ArrayList(u8),

    pub fn init(alloc: std.mem.Allocator) SaveMemory {
        return .{
            .memory = std.ArrayList(u8).init(alloc),
            .strings = std.ArrayList(u8).init(alloc),
        };
    }

    pub fn deinit(self: *SaveMemory) void {
        self.memory.deinit();
        self.strings.deinit();
    }

    pub fn intern(self: *SaveMemory, string: []const u8) !u32 {
        if (std.mem.indexOf(u8, self.strings.items, string)) |index| {
            return @intCast(index);
        }
        const offset = self.strings.items.len;
        try self.strings.appendSlice(string);
        return @intCast(offset);
    }

    pub fn write_header(self: *SaveMemory, tag: *const [4]u8) !u32 {
        const offset = self.memory.items.len;
        try self.write(u32, 0);
        _ = try self.memory.appendSlice(tag);

        return @intCast(offset);
    }

    pub fn write_header_length(self: *SaveMemory, offset: u32) !void {
        const new_offset: u32 = @intCast(self.memory.items.len);
        const length: u32 = new_offset - offset;
        try self.write_u32_at(@intCast(offset), @intCast(length));
    }

    pub fn write(self: *SaveMemory, T: type, value: T) !void {
        _ = try self.memory.appendSlice(std.mem.asBytes(&value));
    }

    pub fn write_string(self: *SaveMemory, string: []const u8) !void {
        const interned = try self.intern(string);
        const len: u32 = @intCast(string.len);

        try self.write_int(interned);
        try self.write_int(len);
    }

    pub fn write_int(self: *SaveMemory, value: usize) !void {
        if (value <= 0x7F) {
            try self.write(u8, @intCast(value));
        } else if (value <= 0x3FFF) {
            const new_value: u16 = @intCast(value | 0x8000);
            try self.write(u16, @byteSwap(new_value));
        } else if (value <= 0x1FFF_FFFF) {
            const new_value: u32 = @intCast(value | 0xC000_0000);
            try self.write(u32, @byteSwap(new_value));
        } else if (value <= 0xFFFF_FFFF) {
            const new_value: u32 = @intCast(value);
            try self.write(u8, 0xF4);
            try self.write(u32, @byteSwap(new_value));
        } else {
            const new_value: u64 = @intCast(value);
            try self.write(u8, 0xF8);
            try self.write(u64, @byteSwap(new_value));
        }
    }

    pub fn write_u32_at(self: *SaveMemory, offset: u32, value: u32) !void {
        const data = std.mem.asBytes(&value);
        self.memory.items[offset + 0] = data[0];
        self.memory.items[offset + 1] = data[1];
        self.memory.items[offset + 2] = data[2];
        self.memory.items[offset + 3] = data[3];
    }

    pub fn save_strings(self: *SaveMemory) !u32 {
        const offset = try self.write_header("STR:");

        _ = try self.memory.appendSlice(self.strings.items);

        try self.write_header_length(offset);

        return offset;
    }

    pub fn toOwnedSlice(self: *SaveMemory) ![]u8 {
        const memory = try self.memory.toOwnedSlice();
        self.strings.deinit();

        return memory;
    }
};

pub fn save(alloc: std.mem.Allocator) ![]u8 {
    var memory = SaveMemory.init(alloc);
    errdefer memory.deinit();

    const head_offset = try memory.write_header("HEAD");
    try memory.write(u32, 0);

    inline for (std.meta.fields(ComponentFieldKind)) |field| {
        try memory.write_int(field.value);
        try memory.write_string(field.name);
    }
    try memory.write_header_length(head_offset);

    try save_component_info(&memory);

    try save_entity_info(&memory);

    try save_recycled(&memory);

    const strings_offset = try memory.save_strings();
    try memory.write_u32_at(head_offset + 8, strings_offset);

    return memory.toOwnedSlice();
}

fn save_component_info(memory: *SaveMemory) !void {
    for (0.., components.items) |type_id, component| {
        const offset = try memory.write_header("COMP");

        try memory.write_int(type_id);

        try memory.write_string(component.name);

        try memory.write_int(@intCast(component.size));

        switch (component.info) {
            .@"struct" => |info| {
                try memory.write_int(0);
                try memory.write_int(info.num_fields);
                for (0..info.num_fields) |i| {
                    const field = info.fields[i];
                    try memory.write_string(field.name);
                    try memory.write_int(field.offset);
                    try memory.write_int(field.size);
                    try memory.write_int(@intFromEnum(field.kind));
                }
            },
            .@"enum" => |info| {
                try memory.write_int(1);
                try memory.write_int(@intFromEnum(info.tag_type));
                try memory.write_int(info.num_values);

                for (0..info.num_values) |i| {
                    const enum_value = info.values[i];
                    try memory.write_int(enum_value.value);
                    try memory.write_string(enum_value.name);
                }
            },
        }

        try memory.write_header_length(offset);
    }
}

fn save_entity_info(memory: *SaveMemory) !void {
    for (0.., generations.items) |idx, gen| {
        const offset = try memory.write_header("ENTT");
        const ent = Entity{ .gen = gen, .idx = @intCast(idx) };
        _ = try memory.write(Entity, ent);
        for (0.., components.items) |type_id, component| {
            if (component.get_data(ent)) |data| {
                try memory.write_int(type_id);
                try memory.write_int(data.len);
                _ = try memory.memory.appendSlice(data);
            }
        }
        try memory.write_header_length(offset);
    }
}

fn save_recycled(memory: *SaveMemory) !void {
    const recycled_offset = try memory.write_header("RCYC");
    for (recycled.items) |ent| {
        try memory.write_int(ent);
    }
    try memory.write_header_length(recycled_offset);

    const old_recycled_offset = try memory.write_header("OLDR");
    for (old_recycled.items) |ent| {
        try memory.write_int(ent);
    }
    try memory.write_header_length(old_recycled_offset);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const DumpOptions = packed struct(u8) {
    components: bool = false,
    entities: bool = false,
    entity_data: bool = false,
    systems: bool = false,
    _: u4 = 0,

    pub const all: DumpOptions = .{
        .components = true,
        .entities = true,
        .entity_data = true,
        .systems = true,
    };
};

pub fn dump(options: DumpOptions) void {
    std.debug.assert(initialized);
    log.msg("Dump ECS", .{});
    log.inc_depth();
    defer log.dec_depth();

    if (options.components) {
        log.msg("Components", .{});
        log.inc_depth();
        defer log.dec_depth();
        for (components.items) |component| {
            log.msg("{s} {s} ({} bytes)", .{
                @tagName(component.info),
                component.name,
                component.size,
            });
            log.inc_depth();
            defer log.dec_depth();
            switch (component.info) {
                .@"struct" => |info| {
                    for (0..info.num_fields) |i| {
                        const field = info.fields[i];
                        log.msg("{s}: {s} (offset {}, {} bytes)", .{
                            field.name,
                            @tagName(field.kind),
                            field.offset,
                            field.size,
                        });
                    }
                },
                .@"enum" => |info| {
                    for (0..info.num_values) |i| {
                        const value = info.values[i];
                        log.msg("{s} = {}", .{
                            value.name,
                            value.value,
                        });
                    }
                },
            }
        }
    }

    if (options.systems) {
        log.msg("Systems", .{});
        log.inc_depth();
        defer log.dec_depth();
        for (systems.items) |sys| {
            var buffer = std.ArrayList(u8).init(allocator);
            defer buffer.deinit();
            var writer = buffer.writer();
            writer.print(
                "{s} {s} {d:.6} (",
                .{ sys.name, @tagName(sys.phase), sys.last_elapsed },
            ) catch {};
            var joint = false;
            for (0..sys.num_arguments) |i| {
                const type_id = sys.arguments[i];
                const info = components.items[type_id];
                if (joint) {
                    writer.print(", ", .{}) catch {};
                }
                if (sys.mutable[i]) {
                    writer.print("*", .{}) catch {};
                }
                writer.print("{s}", .{info.name}) catch {};
                joint = true;
            }
            writer.print(")", .{}) catch {};
            log.msg("{s}", .{buffer.items});

            log.inc_depth();
            defer log.dec_depth();

            const system_entities = sys.entities.keys();
            log.msg("System Entities ({})", .{system_entities.len});
            log.inc_depth();
            defer log.dec_depth();
            for (system_entities) |ent| {
                log.msg("{}", .{ent});
            }
        }
    }

    if (options.entities) {
        log.msg("Entities", .{});
        for (0.., generations.items) |idx, gen| {
            const ent = Entity{ .gen = gen, .idx = @intCast(idx) };
            log.msg("  Entity {}", .{ent});
            if (options.entity_data) {
                for (components.items) |component| {
                    dump_entity_component("", ent, component);
                }
            }
        }
    }
}

fn dump_entity_component(
    prefix: []const u8,
    ent: Entity,
    component: ComponentInfo,
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
                    else => {},
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

    try std.testing.expectEqual(Entity{ .gen = 0, .idx = 0 }, e0);
    try std.testing.expectEqual(Entity{ .gen = 0, .idx = 1 }, e1);
    try std.testing.expectEqual(Entity{ .gen = 0, .idx = 2 }, e2);
    try std.testing.expectEqual(Entity{ .gen = 0, .idx = 3 }, e3);

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

    try std.testing.expectEqual(Entity{ .gen = 0, .idx = 0 }, e0);
    try std.testing.expectEqual(Entity{ .gen = 0, .idx = 1 }, e1);
    try std.testing.expectEqual(Entity{ .gen = 0, .idx = 2 }, e2);
    try std.testing.expectEqual(Entity{ .gen = 0, .idx = 3 }, e3);

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

    try std.testing.expectEqual(Entity{ .gen = 0, .idx = 0 }, e0);
    try std.testing.expectEqual(Entity{ .gen = 0, .idx = 1 }, e1);
    try std.testing.expectEqual(Entity{ .gen = 0, .idx = 2 }, e2);
    try std.testing.expectEqual(Entity{ .gen = 0, .idx = 3 }, e3);
    try std.testing.expectEqual(Entity{ .gen = 1, .idx = 3 }, e4);
    try std.testing.expectEqual(Entity{ .gen = 1, .idx = 2 }, e5);
    try std.testing.expectEqual(Entity{ .gen = 1, .idx = 1 }, e6);
    try std.testing.expectEqual(Entity{ .gen = 1, .idx = 0 }, e7);
    try std.testing.expectEqual(Entity{ .gen = 0, .idx = 4 }, e8);
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

    try std.testing.expectEqual(Entity{ .gen = 0, .idx = 0 }, e0);
    try std.testing.expectEqual(false, e0.is_valid());

    for (0..254) |_| {
        e0 = new();
        e0.delete();
    }

    e0 = new();
    try std.testing.expectEqual(Entity{ .gen = 0, .idx = 1 }, e0);
    try std.testing.expectEqual(true, e0.is_valid());

    for (0..254) |i| {
        const e = Entity{ .gen = @intCast(i), .idx = 0 };
        try std.testing.expectEqual(false, e.is_valid());
    }

    recycle_old_generations();

    const e1 = new();

    try std.testing.expectEqual(Entity{ .gen = 0, .idx = 0 }, e1);
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

var test_simple_system_counter: usize = 0;

test "simple system" {
    init(std.testing.allocator);
    defer deinit();

    const simple_system = (struct {
        fn simple_system(iter: *const SystemIterator) void {
            _ = iter;
            test_simple_system_counter += 1;
        }
    }).simple_system;

    register_system(.{ .name = "Simple System" }, simple_system, .{});

    progress(1);
    progress(1);

    try std.testing.expectEqual(2, test_simple_system_counter);
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
        fn movement_system(iter: *const SystemIterator) void {
            for (iter.entities) |entity| {
                if (entity.getPtr(Position)) |pos| {
                    if (entity.get(Velocity)) |vel| {
                        pos.x = pos.x + vel.dx * iter.delta_time;
                        pos.y = pos.y + vel.dy * iter.delta_time;
                    }
                }
            }
        }
    }).movement_system;

    register_system(
        .{ .name = "Movement System" },
        movement_system,
        .{ *Position, Velocity },
    );

    const e0 = new();
    const e1 = new();

    e0.set(Position{ .x = 2, .y = 3 });
    e0.set(Velocity{ .dx = 0.2, .dy = -0.1 });

    e1.set(Position{ .x = 4, .y = 5 });
    e1.set(Velocity{ .dx = 0.1, .dy = -0.2 });

    progress(1);
    progress(1);

    dump(.all);

    const v0 = e0.get(Velocity) orelse return error.EntityHasNoVelocity;
    const v1 = e1.get(Velocity) orelse return error.EntityHasNoVelocity;

    const p0 = e0.get(Position) orelse return error.EntityHasNoPosition;
    const p1 = e1.get(Position) orelse return error.EntityHasNoPosition;

    const epsilon = 0.00001;

    try std.testing.expectApproxEqAbs(0.2, v0.dx, epsilon);
    try std.testing.expectApproxEqAbs(-0.1, v0.dy, epsilon);

    try std.testing.expectApproxEqAbs(0.1, v1.dx, epsilon);
    try std.testing.expectApproxEqAbs(-0.2, v1.dy, epsilon);

    try std.testing.expectApproxEqAbs(2.4, p0.x, epsilon);
    try std.testing.expectApproxEqAbs(2.8, p0.y, epsilon);

    try std.testing.expectApproxEqAbs(4.2, p1.x, epsilon);
    try std.testing.expectApproxEqAbs(4.6, p1.y, epsilon);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
