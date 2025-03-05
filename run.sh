#!/usr/bin/sh

# zig fmt .
time -f "%e seconds" zig build run --color on -freference-trace=32 --summary none -Dcpu=broadwell
