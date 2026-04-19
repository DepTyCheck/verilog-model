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

import logging
import sys
from pathlib import Path

from common.assets import Assets
from common.command_config import parse_commands
from common.language_config import get_file_extension, load_language_config
from common.logger import configure_logger, get_logger
from common.tool_matrix_runner import ResultCollector, run_all
from common.unknown_error_reporter import handle_unknown_errors
from regression_test.src.error_checker import iter_regression_inputs, load_all_error_files
from regression_test.src.parse_args import parse_args
from regression_test.src.result_reporter import build_reproducibility_report, format_markdown_table, save_report
from tools_run.src.ignored_errors_list import IgnoredErrorsList


def main() -> None:
    configure_logger(level=logging.DEBUG)
    configure_logger(name="regression_test", level=logging.DEBUG)
    args = parse_args()

    commands = parse_commands(args.commands_json)
    language_extensions = load_language_config(args.language_config)
    file_suffix = get_file_extension(args.language, language_extensions)
    assets = Assets(args.assets) if args.assets else None

    logger = get_logger("regression_test")
    known_errors_path = Path(args.known_errors_dir)
    logger.debug(f"known_errors_dir resolved: {known_errors_path.resolve()}")
    logger.debug(f"known_errors_dir exists: {known_errors_path.exists()}")
    if known_errors_path.exists():
        subdirs = [p.name for p in sorted(known_errors_path.iterdir()) if p.is_dir()]
        logger.debug(f"known_errors_dir subdirs ({len(subdirs)}): {subdirs}")
    else:
        logger.warning(f"known_errors_dir does not exist: {known_errors_path.resolve()}")

    error_files = load_all_error_files(args.known_errors_dir)
    logger.debug(f"Loaded {len(error_files)} error files total")
    if not error_files:
        logger.warning("No error files loaded — nothing to test")

    all_known_errors = IgnoredErrorsList.from_error_files(error_files, extra_regexes=args.extra_ignored_regexes)

    collector = ResultCollector()
    run_all(
        iter_regression_inputs(error_files, file_suffix, assets=assets, language=args.language),
        commands,
        all_known_errors,
        collector,
    )

    results = collector.results()

    # ── shared phase ────────────────────────────────────────────────────────
    unknown_entries = handle_unknown_errors(results, args.unknown_errors_output)

    # ── regression_test-specific phase ───────────────────────────────────────
    report = build_reproducibility_report(results, args.tool_name)
    save_report(report, args.output)
    print(format_markdown_table(report, error_url_prefix=args.error_url_prefix))

    sys.exit(1 if unknown_entries else 0)


if __name__ == "__main__":
    main()
