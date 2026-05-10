#!/usr/bin/env python3
"""Read ci/conf/tools.yaml and emit GHA matrix JSON to $GITHUB_OUTPUT."""

import json
import os
from pathlib import Path

from gen_matrix.gen_matrix import build_matrix, load_tools


def main() -> None:
    # ci/runner/gen_matrix/main.py -> up three levels -> ci/conf/tools.yaml
    tools_yaml = Path(__file__).parents[2] / "conf" / "tools.yaml"

    tools = load_tools(str(tools_yaml))
    matrix_json = json.dumps(build_matrix(tools))

    github_output = os.environ.get("GITHUB_OUTPUT")
    if github_output:
        with open(github_output, "a", encoding="utf-8") as f:
            f.write(f"matrix={matrix_json}\n")
    else:
        print(f"matrix={matrix_json}")


if __name__ == "__main__":
    main()
