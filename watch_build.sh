#!/usr/bin/sh

zig build \
    --watch \
    --color on \
    -freference-trace=32 \
    -Dcpu=broadwell \
    --summary all
