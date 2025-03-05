#!/usr/bin/sh

zig fmt --check . || { echo "zig fmt check failed"; exit 1; }
echo "zig fmt check complete"
fossil commit -m "stuff"
fossil git export
