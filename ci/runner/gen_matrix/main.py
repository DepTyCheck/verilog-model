#!/usr/bin/env python3
"""Read a tools config YAML and emit GHA matrix JSON to $GITHUB_OUTPUT."""

import argparse
import json
import os

from gen_matrix.gen_matrix import build_matrix, load_tools


def main() -> None:
    parser = argparse.ArgumentParser(description="Emit a GHA tool matrix from a tools config YAML.")
    parser.add_argument("--tools-config", type=str, required=True, help="Path to tools.yaml")
    args = parser.parse_args()

    tools = load_tools(args.tools_config)
    matrix_json = json.dumps(build_matrix(tools))

    github_output = os.environ.get("GITHUB_OUTPUT")
    if github_output:
        with open(github_output, "a", encoding="utf-8") as f:
            f.write(f"matrix={matrix_json}\n")
    else:
        print(f"matrix={matrix_json}")


if __name__ == "__main__":
    main()
