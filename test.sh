#!/usr/bin/sh

zig build \
    --color on \
    --summary all \
    -Dcpu=broadwell \
    test
