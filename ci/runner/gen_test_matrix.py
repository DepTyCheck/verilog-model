#!/usr/bin/env python3
"""Scan ci/runner for packages with a tests/test.py and emit a GHA matrix JSON.

Discovery rule: any immediate subdirectory that contains tests/test.py is a suite.
If the subdirectory also has requirements.txt, it is included in the matrix entry.

No external dependencies — stdlib only.
"""

import json
import os
from pathlib import Path


def discover_suites(runner_dir: Path) -> list[dict]:
    suites = []
    for entry in sorted(runner_dir.iterdir()):
        if not entry.is_dir() or entry.name.startswith((".", "_")):
            continue
        if not (entry / "tests" / "test.py").exists():
            continue
        suite: dict = {"name": entry.name}
        if (entry / "requirements.txt").exists():
            suite["requirements"] = f"{entry.name}/requirements.txt"
        suites.append(suite)
    return suites


def main() -> None:
    runner_dir = Path(__file__).parent
    suites = discover_suites(runner_dir)
    matrix = json.dumps({"include": [{"suite": s} for s in suites]})

    github_output = os.environ.get("GITHUB_OUTPUT")
    if github_output:
        with open(github_output, "a") as f:
            f.write(f"matrix={matrix}\n")
    else:
        print(f"matrix={matrix}")


if __name__ == "__main__":
    main()
