///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");
const ng = @import("ng");

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Node = extern struct {
    pos: ng.Vec2,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Curve = extern struct {
    from: ng.Entity, // Link
    to: ng.Entity, // Link
    radius: f32,

    center: ng.Vec2 = .{ 0, 0 },
    start_angle: f32 = 0,
    end_angle: f32 = 0,
    clockwise: bool = false,
    width: u16 = 0,
    style: ng.Entity = ng.Entity.nil, // the lane style of this link

    offset: f32 = 0,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const max_junction_arms = 8;

pub const Junction = extern struct {
    radius: f32,
    arms: [max_junction_arms]ng.Entity = undefined,
    num_arms: u8 = 0,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const CurveUpdateRequired = extern struct {};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Link = extern struct {
    start: ng.Entity, // start of the link
    end: ng.Entity, // the end of the link
    width: u16, // the width of the link in 0.1m
    style: ng.Entity, // the lane style of this link
};

pub const max_lanes = 32;

// lanes are left to right when road is from bottom to top
pub const LinkStyle = extern struct {
    width: [max_lanes]u16,
    kind: [max_lanes]LaneKind,
    num_lanes: u8,
    total_width: u16,
};

// lanes are from bottom to top
pub const LaneKind = enum(u8) {
    sidewalk,
    kerb,
    lane_line,
    center_line,
    shared_use,
    cycle_up,
    cycle_down,
    bus_up,
    bus_down,
    traffic_up,
    traffic_down,
    parking_up,
    parking_down,
    shoulder,
    medium,
    barrier,
    dirt,
    grass,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Construction = extern struct {
    step: f32 = 0,
    steps: f32,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const VehicleKind = enum(u8) {
    person,
    bicycle,
    cart,
    bus,
    car,
    truck,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const OnLink = struct {
    on: ng.Entity,
    lane: u8,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Position = extern struct {
    pos: ng.Vec2,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Velocity = extern struct {
    vel: ng.Vec2,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Health = extern struct {
    hp: u8,
    max: u8,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Skill = enum(u8) {
    reading,
    writing,
    math,
    science,
    history,
    charisma,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Person = extern struct {
    first_name: u32,
    middle_name: u32,
    last_name: u32,
};

pub const MaidenName = extern struct {
    last_name: u32,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Single = extern struct {};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Partner = extern struct {
    partner: ng.Entity,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const Random = extern struct {
    rand: u128,
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
