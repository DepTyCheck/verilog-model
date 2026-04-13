"""
Manual integration check for iverilog.

Requirements:
  - iverilog must be installed and available on PATH

Run from ci/runner/:
  python -m regression_test.integration_tests.test_iverilog

Prints a JSON report:
  { "<error_id>": { "<example_name>": true/false } }
where true = known error reproduced, false = not found.
"""

import json
from pathlib import Path

from common.error_file_parser import parse_error_files
from common.tool_error_regex import ToolErrorRegex
from regression_test.src.error_checker import ExampleStatus, ToolConfig, _run_example_with_tool

DATA_DIR = Path(__file__).parent / "data"

IVERILOG_TOOL = ToolConfig(
    name="iverilog",
    cmd="iverilog -g2012 -o /dev/null {file}",
    error_regex=ToolErrorRegex(r"(syntax error\W[A-z-\/0-9,.:]+ .*$|(error|sorry|assert|vvp): [\S ]+$)"),
)


def main() -> None:
    error_files = parse_error_files(str(DATA_DIR), tool="iverilog")
    if not error_files:
        print(f"No iverilog YAML files found in {DATA_DIR}")
        return

    report: dict[str, dict[str, bool]] = {}
    for ef in error_files:
        examples: dict[str, bool] = {}
        for example in ef.examples:
            result = _run_example_with_tool(example, ef, IVERILOG_TOOL)
            examples[example.name] = result.status == ExampleStatus.KNOWN_ERROR
        report[ef.error_id] = examples

    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
