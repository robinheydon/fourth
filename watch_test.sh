#!/usr/bin/sh

time -f "%e seconds (%x exit code)" zig build \
    --watch \
    --color on \
    -Dcpu=broadwell \
    test
