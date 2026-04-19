"""
Manual integration check for iverilog.

Requirements:
  - iverilog must be installed and available on PATH

Run from ci/runner/:
  python -m regression_test.integration_tests.test_iverilog

Prints a JSON report:
  { "<error_id>": { "<example_name>-<type>": true/false } }
where true = known error reproduced, false = not found.
"""

import json
from pathlib import Path

from common.command_config import parse_commands
from common.error_file_parser import parse_error_files
from common.ignored_errors_list import IgnoredErrorsList
from common.tool_matrix_runner import ResultCollector, run_all
from regression_test.src.error_checker import iter_regression_inputs
from regression_test.src.result_reporter import build_reproducibility_report

DATA_DIR = Path(__file__).parent / "data"

_IVERILOG_COMMANDS_JSON = json.dumps(
    [
        {"run": "iverilog -g2012 -o a.out {file}", "error_regex": r"(syntax error\W[A-z-\/0-9,.:]+ .*$|(error|sorry|assert|vvp): [\S ]+$)"},
        {"run": "vvp a.out", "error_regex": r"(syntax error\W[A-z-\/0-9,.:]+ .*$|(error|sorry|assert|vvp): [\S ]+$)"},
    ]
)


def main() -> None:
    error_files = parse_error_files(str(DATA_DIR), tool="iverilog")
    if not error_files:
        print(f"No iverilog YAML files found in {DATA_DIR}")
        return

    commands = parse_commands(_IVERILOG_COMMANDS_JSON)
    all_known_errors = IgnoredErrorsList.from_error_files(error_files)

    collector = ResultCollector()
    run_all(
        iter_regression_inputs(error_files, ".sv", language="sv"),
        commands,
        all_known_errors,
        collector,
    )

    report = build_reproducibility_report(collector.results(), "iverilog")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
