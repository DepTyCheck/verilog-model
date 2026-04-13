#!/usr/bin/env python3
"""
regression_test — regression checker for known tool bugs.

Usage (from ci/runner/):
  python -m regression_test.main \
    --known-errors-dir verilog-gh-pages/found_errors \
    --output regression-test-report.json \
    --tool-name iverilog \
    --commands-json '[{"run": "iverilog -g2012 -o /dev/null {file}", "error_regex": "(syntax error|error): .*"}]' \
    --language sv

Exit codes:
  0 — all checks passed (CLEAN or KNOWN_ERROR only)
  1 — at least one NEW_ERROR was found
"""

import json
import logging
import sys

from common.command_config import CommandConfig
from common.logger import configure_logger
from common.tool_error_regex import ToolErrorRegex
from regression_test.src.error_checker import ToolConfig, check_all
from regression_test.src.parse_args import parse_args
from regression_test.src.result_reporter import build_report, format_markdown_table, print_summary, save_report


def _tool_from_commands_json(tool_name: str, commands_json: str, language: str) -> ToolConfig:
    raw = json.loads(commands_json)
    commands = []
    for entry in raw:
        regex_str = entry.get("error_regex")
        commands.append(
            CommandConfig(
                run=entry["run"],
                error_regex=ToolErrorRegex(regex_str) if regex_str else None,
            )
        )
    return ToolConfig(name=tool_name, commands=commands, language=language)


def main() -> None:
    configure_logger(name="regression_test", level=logging.INFO)

    args = parse_args()

    tool = _tool_from_commands_json(args.tool_name, args.commands_json, args.language)

    error_results, new_error_incidents, regression_confirmations = check_all(
        known_errors_dir=args.known_errors_dir,
        tool=tool,
        extra_ignored_regexes=args.extra_ignored_regexes,
    )

    report = build_report(error_results)
    save_report(report, args.output)
    print_summary(error_results, new_error_incidents, regression_confirmations)
    print(format_markdown_table(report, error_url_prefix=args.error_url_prefix))

    if new_error_incidents:
        sys.exit(1)


if __name__ == "__main__":
    main()
