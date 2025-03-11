///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");
const ng = @import("ng");

const ui = ng.ui;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const HBox = @This();

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const Object = ui.Object;
const Handle = ui.Handle;
const LayoutConstraint = ui.LayoutConstraint;
const Padding = ui.Padding;

const log = ui.log;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub const BeginBoxOptions = struct {
    unique: usize = 0,
    padding: Padding = .{},
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn begin_hbox(options: BeginBoxOptions) void {
    const ident = ui.Ident{ .addr = @returnAddress(), .unique = options.unique };

    const parent = ui.top_build_stack();

    if (ui.find_object(parent, ident)) |handle| {
        var object = ui.get(handle) catch return;
        object.active = true;
        ui.move_child_last (parent, handle);
        ui.push_build_stack(handle);
    } else {
        const handle = ui.new() catch |err| {
            log.err("begin_box {}", .{err});
            return;
        };

        const object = ui.get(handle) catch return;

        object.* = .{
            .ident = ident,
            .active = true,
            .padding = options.padding,
            .data = .{
                .hbox = .{},
            },
        };

        ui.add_child_last(parent, handle);

        ui.push_build_stack(handle);
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub noinline fn end_hbox() void {
    ui.pop_build_stack();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn layout(self: *const HBox, handle: Handle, constraint: LayoutConstraint) ng.Vec2 {
    _ = self;
    const obj = ui.get(handle) catch return .{ 0, 0 };

    var pos = obj.pos + ng.Vec2{ obj.padding.left, obj.padding.top };
    var max_size = ng.Vec2{ 0, 0 };

    var iter = obj.children();
    while (iter.next()) |child| {
        var child_obj = ui.get(child) catch return max_size;

        child_obj.pos = pos;

        const size = child.layout(constraint);
        if (size[1] > max_size[1]) {
            max_size[1] = size[1];
        }

        child_obj.size = size;

        pos[0] += size[0];
        max_size[0] += size[0];
    }

    max_size += ng.Vec2{ obj.padding.left, obj.padding.top };
    max_size += ng.Vec2{ obj.padding.right, obj.padding.bottom };

    return max_size;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn process_event(_: *HBox, _: Handle, _: ng.Event) bool {
    return false;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
