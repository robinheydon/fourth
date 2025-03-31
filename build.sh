#!/usr/bin/sh

zig build \
    --color on \
    -freference-trace=32 \
    -Dcpu=broadwell \
    --summary all

cloc . \
    --exclude-dir=.zig-cache \
    --include-ext=zig
