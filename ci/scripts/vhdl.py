#!/usr/bin/env python3
"""Translate a command template into a concrete VHDL command.

Usage: vhdl.py "<command-template>" "<file-path>"
Substitutes {file} and, when present, {vhdl_top_entity} (the last `entity <name> is`).
"""

import re
import sys


def find_top_entity(content: str) -> str:
    matches = re.findall(r"(?<=entity )\w+(?= is)", content, re.MULTILINE | re.IGNORECASE)
    if not matches:
        raise SystemExit("No top entity found")
    return matches[-1]


def main() -> None:
    template, file_path = sys.argv[1], sys.argv[2]
    out = template
    if "{vhdl_top_entity}" in out:
        with open(file_path, encoding="utf-8") as f:
            out = out.replace("{vhdl_top_entity}", find_top_entity(f.read()))
    out = out.replace("{file}", file_path)
    sys.stdout.write(out)


if __name__ == "__main__":
    main()
