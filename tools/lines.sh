#!/usr/bin/env sh

cloc . \
    --by-file \
    --exclude-dir=.zig-cache
