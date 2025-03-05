#!/usr/bin/sh

time zig build \
    --color on \
    --summary all \
    -freference-trace=32 \
    -Dcpu=broadwell \
    -fincremental
