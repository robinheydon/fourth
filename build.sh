#!/usr/bin/sh

zig build \
    --color on \
    --summary all \
    -freference-trace=32 \
    -Dcpu=broadwell \
    -fincremental
