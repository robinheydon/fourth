#!/usr/bin/sh

# zig fmt .
zig build run \
    --color on \
    -freference-trace=32 \
    -Dcpu=broadwell \
    --summary none
