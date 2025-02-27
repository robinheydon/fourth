#!/usr/bin/env python3

import pprint
import os
import pathlib

def main ():
    dot = pathlib.Path (".")
    for (root, dirs, files) in os.walk (dot):
        for file in files:
            if file.endswith (".zig"):
                filename = dot / root / file
                content = open (filename).read ()
                enabled = True
                lines = content.splitlines ()
                line_number = 1
                for line in lines:
                    stripped_line = line.strip ()
                    if stripped_line == "// zig fmt: off":
                        enabled = False
                    if stripped_line == "// zig fmt: on":
                        enabled = True
                    if enabled and len(line) > 95:
                        print (enabled, filename, line_number, line)
                    line_number += 1


if __name__ == "__main__":
    main ()
