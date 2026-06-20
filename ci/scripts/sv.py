#!/usr/bin/env python3
"""Translate a command template into a concrete SystemVerilog command.

Usage: sv.py "<command-template>" "<file-path>"
Substitutes {file} and, when present, {top_module} (the last `module <name>`).
"""

import re
import sys


def find_top_module(content: str) -> str:
    matches = re.findall(r"(?<=module )\w+", content, re.MULTILINE)
    if not matches:
        raise SystemExit("No top module found")
    return matches[-1]


def main() -> None:
    template, file_path = sys.argv[1], sys.argv[2]
    out = template
    if "{top_module}" in out:
        with open(file_path, encoding="utf-8") as f:
            out = out.replace("{top_module}", find_top_module(f.read()))
    out = out.replace("{file}", file_path)
    sys.stdout.write(out)


if __name__ == "__main__":
    main()
