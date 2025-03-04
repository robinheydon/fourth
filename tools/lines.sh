#!/usr/bin/env sh

cloc . \
    --by-file \
    --exclude-dir=.zig-cache \
    --include-ext=zig,glsl

cloc . \
    --exclude-dir=.zig-cache \
    --include-ext=zig
