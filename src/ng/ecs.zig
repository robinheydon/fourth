///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import ("std");

const ng = @import ("ng");

const log = ng.Logger (.ecs);

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

var gpa: std.heap.GeneralPurposeAllocator (.{}) = undefined;
var allocator: std.mem.Allocator = undefined;

var initialized = false;

var generations: std.ArrayList (EntityGeneration) = undefined;

var components: std.AutoHashMap (TypeId, ComponentInfo) = undefined;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const EntityIndex = u24;
const EntityGeneration = u8;

pub const Entity = enum (u32) {
    null_entity = 0xFFFFFFFF,
    _,

    pub fn set (self: Entity, comptime Component: type, value: Component) void
    {
        log.debug ("set {} {s} {any}", .{self, @typeName (Component), value});
    }

    pub fn delete (self: Entity) void
    {
        log.debug ("delete {}", .{self});
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn init () void
{
    gpa = std.heap.GeneralPurposeAllocator(.{}){};
    allocator = gpa.allocator ();

    generations = std.ArrayList (EntityGeneration).init (allocator);
    components = std.AutoHashMap (TypeId, ComponentInfo).init (allocator);

    initialized = true;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn deinit () void
{
    generations.deinit ();

    std.debug.assert (gpa.deinit () == .ok);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn get_index (entity: Entity) EntityIndex
{
    const ent : u32 = @intFromEnum (entity);

    const index : EntityIndex = ent & 0xFFFFFF;
    return index;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn get_generation (entity: Entity) EntityIndex
{
    const ent = @intFromEnum (entity);

    const gen : EntityGeneration = (ent >> 24) & 0xFF;
    return gen;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const ComponentInfo = struct {
    name: []const u8,
    size: usize,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn register_component (name: []const u8, comptime Component: type) void
{
    const type_id = get_type_id (Component);
    log.info ("register component {s} {s} {x}", .{name, @typeName (Component), type_id});
}

const TypeId = usize;

fn get_type_id (comptime Component: type) TypeId
{
    return @intFromPtr (&(struct {
        var item: u8 = @intCast (@sizeOf (Component));
    }).item);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn new () Entity
{
    return .null_entity;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn dump_ecs () void
{
    log.msg ("Dump ECS", .{});
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
