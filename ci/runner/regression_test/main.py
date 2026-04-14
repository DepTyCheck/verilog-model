#!/usr/bin/env python3
"""
regression_test — regression checker for known tool bugs.

Usage (from ci/runner/):
  python -m regression_test.main \
    --known-errors-dir verilog-gh-pages/found_errors \
    --output regression-test-report.json \
    --tool-name iverilog \
    --commands-json '[{"run": "iverilog -g2012 -o /dev/null {file}", "error_regex": "..."}]' \
    --language sv \
    --language-config ../../conf/languages.yaml

Exit codes:
  0 — no unknown errors found
  1 — at least one unknown error was found
"""

import json
import logging
import sys

from common.assets import Assets
from common.command_config import CommandConfig
from common.language_config import get_file_extension, load_language_config
from common.logger import configure_logger
from common.tool_error_regex import ToolErrorRegex
from common.tool_matrix_runner import ResultCollector, run_all
from common.unknown_error_reporter import collect_unknown_errors, print_unknown_errors, save_unknown_errors_json
from regression_test.src.error_checker import iter_regression_inputs, load_all_error_files
from regression_test.src.parse_args import parse_args
from regression_test.src.result_reporter import build_reproducibility_report, format_markdown_table, save_report
from tools_run.src.ignored_errors_list import IgnoredErrorsList


def _parse_commands(commands_json: str) -> list[CommandConfig]:
    raw = json.loads(commands_json)
    result = []
    for entry in raw:
        regex_str = entry.get("error_regex")
        result.append(
            CommandConfig(
                run=entry["run"],
                error_regex=ToolErrorRegex(regex_str) if regex_str else None,
            )
        )
    return result


def main() -> None:
    configure_logger(name="regression_test", level=logging.INFO)
    args = parse_args()

    commands = _parse_commands(args.commands_json)
    language_extensions = load_language_config(args.language_config)
    file_suffix = get_file_extension(args.language, language_extensions)
    assets = Assets(args.assets) if args.assets else None

    error_files = load_all_error_files(args.known_errors_dir)
    all_known_errors = IgnoredErrorsList.from_error_files(error_files, extra_regexes=args.extra_ignored_regexes)

    collector = ResultCollector()
    run_all(
        iter_regression_inputs(error_files, file_suffix, assets=assets),
        commands,
        all_known_errors,
        collector,
    )

    results = collector.results()

    # ── shared phase ────────────────────────────────────────────────────────
    unknown_entries = collect_unknown_errors(results)
    print_unknown_errors(unknown_entries)
    if unknown_entries and args.unknown_errors_output:
        save_unknown_errors_json(unknown_entries, args.unknown_errors_output)

    # ── regression_test-specific phase ───────────────────────────────────────
    report = build_reproducibility_report(results, args.tool_name)
    save_report(report, args.output)
    print(format_markdown_table(report, error_url_prefix=args.error_url_prefix))

    sys.exit(1 if unknown_entries else 0)


if __name__ == "__main__":
    main()
