///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const std = @import("std");
const ng = @import("ng");

const log = ng.Logger(.http);

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

var http_server_thread: std.Thread = undefined;
var number_requests: usize = 0;

var pool: std.Thread.Pool = undefined;

const RequestFunction = *const fn (
    request: *std.http.Server.Request,
    uri: *const std.Uri,
    response: *Response,
) void;

var paths: std.StringHashMap(RequestFunction) = undefined;

var gpa: std.heap.GeneralPurposeAllocator(.{}) = undefined;
var allocator: std.mem.Allocator = undefined;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn init() !void {
    gpa = std.heap.GeneralPurposeAllocator(.{}){};
    allocator = gpa.allocator();

    paths = std.StringHashMap(RequestFunction).init(allocator);

    try paths.put("/", do_index);
    try paths.put("/index.html", do_index);
    try paths.put("/entity", do_entity);
    try paths.put("/component", do_component);
    try paths.put("/system", do_system);

    log.debug("init", .{});

    try pool.init(.{
        .allocator = allocator,
    });

    for (pool.threads) |thread| {
        thread.detach();
    }

    http_server_thread = try std.Thread.spawn(.{}, server, .{});
    try http_server_thread.setName("http server");
    http_server_thread.detach();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

pub fn deinit() void {}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn server() !void {
    const thread_id = std.Thread.getCurrentId();

    log.debug("server start {}", .{thread_id});

    const addr = try std.net.Address.parseIp("127.0.0.1", 4237);
    log.debug("listening at http://{}/ ({})", .{ addr, thread_id });

    var listener = try addr.listen(.{
        .reuse_address = true,
    });

    while (true) {
        const connection = try listener.accept();

        log.debug("accept {} {} ({})", .{
            connection.stream.handle,
            connection.address,
            thread_id,
        });

        try pool.spawn(process_connection, .{connection});
    }

    log.debug("server stop {}", .{thread_id});
    listener.deinit();
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_connection(connection: std.net.Server.Connection) void {
    var buffer: [64 * 1024]u8 = undefined;

    defer connection.stream.close();

    var http_server = std.http.Server.init(connection, &buffer);
    while (true) {
        var request = http_server.receiveHead() catch |err|
            {
                log.err("http request {}", .{err});
                return;
            };

        process_request(&request) catch |err|
            {
                log.err("process request {}", .{err});
                return;
            };
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn process_request(request: *std.http.Server.Request) !void {
    number_requests += 1;

    const uri = try std.Uri.parseAfterScheme("http", request.head.target);
    var path_buffer: [256]u8 = undefined;
    const path = std.Uri.percentDecodeBackwards(&path_buffer, uri.path.percent_encoded);

    var response = Response.init(allocator);
    defer response.deinit();

    if (paths.get(path)) |fun| {
        fun(request, &uri, &response);
    } else {
        do_404(request, &uri, &response);
    }

    request.respond(response.buffer.items, .{
        .status = response.status,
        .extra_headers = response.headers.items,
    }) catch |err|
        {
            log.err("request response {}", .{err});
        };

    log.debug("{s} \"{}\" {d:0>3} {} ({})", .{
        @tagName(request.head.method),
        std.zig.fmtEscapes(request.head.target),
        @intFromEnum(response.status),
        response.buffer.items.len,
        std.Thread.getCurrentId(),
    });
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn do_404(
    request: *std.http.Server.Request,
    uri: *const std.Uri,
    response: *Response,
) void {
    _ = uri;

    response.set_html();
    response.append(html_header);
    response.append(header_div);
    response.print(http_404_response_text, .{request.head.target});
    response.append(html_footer);
    response.set_status(.not_found);
}

const http_404_response_text =
    \\Invalid request {s}
    \\
;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn parse_query(alloc: std.mem.Allocator, query: ?std.Uri.Component) ?Query {
    if (query == null) return null;

    var result = Query.init(alloc);

    switch (query.?) {
        .percent_encoded => |encoded| {
            var parts = std.mem.splitScalar(u8, encoded, '&');
            while (parts.next()) |part| {
                if (std.mem.indexOfScalar(u8, part, '=')) |offset| {
                    result.append(.{
                        .name = part[0..offset],
                        .value = part[offset + 1 ..],
                    }) catch |err| {
                        log.err("query {}", .{err});
                    };
                } else if (part.len > 0) {
                    result.append(.{
                        .name = part,
                        .value = "",
                    }) catch |err| {
                        log.err("query {}", .{err});
                    };
                }
            }
        },
        .raw => |raw| {
            _ = raw;
        },
    }
    return result;
}

const Query = struct {
    kvs: std.ArrayList(std.http.Header),

    pub fn init(alloc: std.mem.Allocator) Query {
        return .{
            .kvs = std.ArrayList(std.http.Header).init(alloc),
        };
    }

    pub fn append(self: *Query, kv: std.http.Header) !void {
        try self.kvs.append(kv);
    }

    pub fn get(self: *Query, name: []const u8) ?[]const u8 {
        for (self.kvs.items) |kv| {
            if (std.mem.eql(u8, kv.name, name)) {
                return kv.value;
            }
        }
        return null;
    }

    pub fn deinit(self: *Query) void {
        self.kvs.deinit();
    }

    pub fn format(self: *Query, _: anytype, _: anytype, writer: anytype) !void {
        try writer.print("Query (", .{});
        for (self.kvs.items) |kv| {
            try writer.print("{s} = {s}, ", .{ kv.name, kv.value });
        }
        try writer.print(")", .{});
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn parse_int(T: type, str: []const u8) ?T {
    return std.fmt.parseInt(T, str, 0) catch return null;
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const Response = struct {
    buffer: std.ArrayList(u8),
    headers: std.ArrayList(std.http.Header),
    status: std.http.Status,

    pub fn init(alloc: std.mem.Allocator) Response {
        return .{
            .buffer = std.ArrayList(u8).init(alloc),
            .headers = std.ArrayList(std.http.Header).init(alloc),
            .status = .ok,
        };
    }

    pub fn deinit(self: *Response) void {
        self.buffer.deinit();
        self.headers.deinit();
    }

    pub fn set_status(self: *Response, status: std.http.Status) void {
        self.status = status;
    }

    pub fn set_html(self: *Response) void {
        self.add_header("Content-Type", "text/html");
    }

    pub fn append(self: *Response, slice: []const u8) void {
        self.buffer.appendSlice(slice) catch |err|
            {
                log.err("response append {}", .{err});
            };
    }

    pub fn add_header(self: *Response, name: []const u8, value: []const u8) void {
        for (self.headers.items) |*header| {
            if (std.mem.eql(u8, header.name, name)) {
                header.value = value;
                return;
            }
        }

        self.headers.append(.{ .name = name, .value = value }) catch |err|
            {
                log.err("response headers append {}", .{err});
            };
    }

    pub fn print(self: *Response, comptime fmt: []const u8, args: anytype) void {
        const writer = self.buffer.writer();

        writer.print(fmt, args) catch |err|
            {
                log.err("response print {}", .{err});
            };
    }
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const html_header =
    \\<!DOCTYPE html>
    \\<html>
    \\<head>
    \\<title>Fourth ECS</title>
    \\<style>
    \\  body {
    \\    background-color: #000;
    \\    color: #fff;
    \\  }
    \\  div#header {
    \\    background-color: #110;
    \\    border-bottom: 1px solid #f80;
    \\  }
    \\  div#container {
    \\    display: table;
    \\    width: 100%;
    \\    border-bottom: 1px solid #f80;
    \\  }
    \\  div#row {
    \\    width: 100%;
    \\  }
    \\  div#left {
    \\    display: table-cell;
    \\    background-color: #101;
    \\    border-right: 1px solid #f80;
    \\  }
    \\  div#right {
    \\    display: table-cell;
    \\    background-color: #011;
    \\  }
    \\</style>
    \\</head>
    \\<body>
    \\
;

const header_div =
    \\<div id="header">
    \\  <a href="/">Home</a>
    \\  <a href="/entity">Entity</a>
    \\  <a href="/component">Component</a>
    \\  <a href="/system">System</a>
    \\</div>
    \\
;

const html_footer =
    \\</body>
    \\</html>
    \\
;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn do_index(
    request: *std.http.Server.Request,
    uri: *const std.Uri,
    response: *Response,
) void {
    _ = request;
    _ = uri;

    response.set_html();
    response.append(html_header);
    response.append(header_div);
    response.append(index_html);
    response.append(html_footer);
}

const index_html =
    \\<div id="container">
    \\  <div id="row">
    \\    <div id="left">
    \\    </div>
    \\    <div id="right">
    \\    </div>
    \\  </div>
    \\</div>
;

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

fn do_entity(
    request: *std.http.Server.Request,
    uri: *const std.Uri,
    response: *Response,
) void {
    _ = request;

    response.set_html();
    response.append(html_header);
    response.append(header_div);

    response.append("<div id=\"container\">\n");
    response.append("<div id=\"row\">\n");

    response.append("<div id=\"left\">\n");

    var iter = ng.entity_iterator();
    while (iter.next()) |entity| {
        const eid: u32 = @bitCast(entity);
        response.print("<a href=\"/entity?eid={}\">{}</a><br>\n", .{ eid, entity });
    }

    response.append("</div>\n");
    response.append("<div id=\"right\">\n");

    var optional_query = parse_query(allocator, uri.query);

    if (optional_query) |*query| {
        defer query.deinit();

        if (query.get("eid")) |eid_str| {
            if (parse_int(u32, eid_str)) |eid| {
                const entity: ng.Entity = @bitCast(eid);
                response.print("<b>{}</b><br>\n", .{entity});

                var component_iter = ng.component_iterator();
                while (component_iter.next()) |component| {
                    if (component.get_data(entity)) |data| {
                        response.print("<b>{s}</b><br>\n", .{component.name});
                        response.print("{any}<br>\n", .{data});
                    }
                }
            }
        }
    }

    response.append("</div>\n");

    response.append("</div>\n");
    response.append("</div>\n");

    response.append(html_footer);
}

fn do_component(
    request: *std.http.Server.Request,
    uri: *const std.Uri,
    response: *Response,
) void {
    _ = request;

    response.set_html();
    response.append(html_header);
    response.append(header_div);
    response.append("<div id=\"container\">\n");
    response.append("<div id=\"row\">\n");

    response.append("<div id=\"left\">\n");

    var iter = ng.component_iterator();
    while (iter.next()) |component| {
        response.print("<a href=\"/component?cid={}\">{s}</a><br>\n", .{
            component.type_id_ptr.*,
            component.name,
        });
    }

    response.append("</div>\n");

    response.append("<div id=\"right\">\n");
    var optional_query = parse_query(allocator, uri.query);

    if (optional_query) |*query| {
        defer query.deinit();

        if (query.get("cid")) |cid_str| {
            if (parse_int(u32, cid_str)) |typeid| {
                if (ng.get_component(typeid)) |component| {
                    response.print("<b>{s}</b><br>\n", .{component.name});
                    response.print("size: {} octets<br>\n", .{component.size});
                    switch (component.info) {
                        .@"struct" => |info| {
                            response.print("struct:<br>\n<ul>\n", .{});
                            for (info.fields[0..info.num_fields]) |field| {
                                if (field.kind == .Component) {
                                    if (ng.get_component(field.type_id)) |field_com| {
                                        if (field.array_len == 1) {
                                            response.print("<li>{s} : {s}\n", .{
                                                field.name,
                                                field_com.name,
                                            });
                                        } else {
                                            response.print("<li>{s} : [{}]{s}\n", .{
                                                field.name,
                                                field.array_len,
                                                field_com.name,
                                            });
                                        }
                                    } else {
                                        if (field.array_len == 1) {
                                            response.print("<li>{s} : {s}\n", .{
                                                field.name,
                                                @tagName(field.kind),
                                            });
                                        } else {
                                            response.print("<li>{s} : [{}]{s}\n", .{
                                                field.name,
                                                field.array_len,
                                                @tagName(field.kind),
                                            });
                                        }
                                    }
                                } else {
                                    if (field.array_len == 1) {
                                        response.print("<li>{s} : {s}\n", .{
                                            field.name,
                                            @tagName(field.kind),
                                        });
                                    } else {
                                        response.print("<li>{s} : [{}]{s}\n", .{
                                            field.name,
                                            field.array_len,
                                            @tagName(field.kind),
                                        });
                                    }
                                }
                            }
                            response.print("</ul>\n", .{});
                        },
                        .@"enum" => |info| {
                            response.print("enum:<br>\n<ul>\n", .{});
                            for (info.values[0..info.num_values]) |value| {
                                response.print("<li>{s} = {}\n", .{
                                    value.name,
                                    value.value,
                                });
                            }
                            response.print("</ul>\n", .{});
                        },
                    }
                }
            }
        }
    }

    response.append("</div>\n");

    response.append("</div>\n");
    response.append("</div>\n");
    response.append(html_footer);
}

fn do_system(
    request: *std.http.Server.Request,
    uri: *const std.Uri,
    response: *Response,
) void {
    _ = request;

    response.set_html();
    response.append(html_header);
    response.append(header_div);
    response.append("<div id=\"container\">");
    response.append("<div id=\"row\">");

    response.append("<div id=\"left\">");

    var iter = ng.system_iterator();
    while (iter.next()) |entry| {
        const system = entry.system;
        response.print("<a href=\"/system?sid={}\">{s}</a><br>\n", .{
            entry.index,
            system.name,
        });
    }

    response.append("</div>");

    response.append("<div id=\"right\">");

    var optional_query = parse_query(allocator, uri.query);

    if (optional_query) |*query| {
        defer query.deinit();

        if (query.get("sid")) |sid_str| {
            if (parse_int(usize, sid_str)) |index| {
                if (ng.get_system(index)) |system| {
                    response.print("<b>{s}</b><br>\n", .{system.name});
                    response.print("phase: {s}<br>\n", .{@tagName(system.phase)});
                    if (system.interval > 0) {
                        response.print("interval: {d}<br>\n", .{system.interval});
                        response.print("wait_time: {d}<br>\n", .{system.wait_time});
                    }
                    response.print("elapsed: {d}<br>\n", .{system.last_elapsed});

                    const ents = system.entities.keys();
                    response.print("entities: {}<br>\n", .{ents.len});
                    for (ents) |ent| {
                        response.print("{}<br>\n", .{ent});
                    }
                }
            }
        }
    }
    response.append("</div>");

    response.append("</div>");
    response.append("</div>");
    response.append(html_footer);
}

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
