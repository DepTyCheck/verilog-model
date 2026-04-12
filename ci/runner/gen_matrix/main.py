#!/usr/bin/env python3
"""Read ci/conf/tools.yaml and emit GHA matrix JSON to $GITHUB_OUTPUT.

Two outputs are emitted:
  matrix        — all tools (consumed by run-tools)
  check-matrix  — only tools with a check_cmd field (consumed by check-known-errors)
"""

import json
import os
from pathlib import Path

from gen_matrix.gen_matrix import build_matrix, load_tools


def main() -> None:
    # ci/runner/gen_matrix/main.py -> up three levels -> ci/conf/tools.yaml
    tools_yaml = Path(__file__).parents[2] / "conf" / "tools.yaml"

    tools = load_tools(str(tools_yaml))
    matrix = build_matrix(tools)
    check_matrix = build_matrix(tools)

    matrix_json = json.dumps(matrix)
    check_matrix_json = json.dumps(check_matrix)

    github_output = os.environ.get("GITHUB_OUTPUT")
    if github_output:
        with open(github_output, "a") as f:
            f.write(f"matrix={matrix_json}\n")
            f.write(f"check-matrix={check_matrix_json}\n")
    else:
        print(f"matrix={matrix_json}")
        print(f"check-matrix={check_matrix_json}")


if __name__ == "__main__":
    main()
