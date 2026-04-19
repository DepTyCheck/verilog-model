#!/usr/bin/env python3

import logging
import sys
from pathlib import Path

from common.command_config import parse_commands
from common.logger import configure_logger, get_logger
from common.tool_matrix_runner import ResultCollector, run_all
from common.unknown_error_reporter import handle_unknown_errors
from tools_run.src.assets import Assets
from tools_run.src.ignored_errors_list import IgnoredErrorsList
from tools_run.src.input_iter import iter_test_files
from tools_run.src.known_errors_report import KnownErrorsReport
from tools_run.src.parse_args import parse_args
from tools_run.src.print_stats import count_run_stats, print_issues_count


def main() -> None:
    configure_logger(level=logging.DEBUG)
    args = parse_args()

    known_errors = IgnoredErrorsList(
        args.ignored_errors_dir,
        args.tool_name,
        regex_list=args.extra_ignored_regexes,
    )
    commands = parse_commands(args.commands_json)
    assets = Assets(args.assets)

    # Determine file suffix from the file pattern (e.g. "*.sv" → ".sv")
    file_suffix = Path(args.file_pattern).suffix or ".sv"

    gen_path = Path(args.gen_path)
    logger = get_logger()
    logger.debug(f"gen_path resolved: {gen_path.resolve()}")
    logger.debug(f"gen_path exists: {gen_path.exists()}")
    if gen_path.exists():
        logger.debug(f"gen_path contents: {[p.name for p in list(gen_path.iterdir())[:20]]}")
    else:
        logger.warning(f"gen_path does not exist: {gen_path.resolve()}")

    files = list(gen_path.glob(args.file_pattern))
    logger.debug(f"Files matched by {args.file_pattern!r}: {len(files)}")
    if not files:
        logger.warning(f"No files found matching {args.file_pattern!r} in {gen_path.resolve()}")

    inputs = iter_test_files(files, file_suffix, assets)

    collector = ResultCollector()
    run_all(inputs, commands, known_errors, collector)

    # ── shared phase ────────────────────────────────────────────────────────
    results = collector.results()
    unknown_entries = handle_unknown_errors(results, args.unknown_errors_output)

    # ── tools_run-specific phase ─────────────────────────────────────────────
    matches = [m for _, r in results for m in r.found_matches]
    report = KnownErrorsReport(commit=args.commit)
    report.add_errors(matches)
    report.save(args.run_statistics_output)

    stats = count_run_stats(results)
    print_issues_count(stats)

    sys.exit(1 if unknown_entries else 0)


if __name__ == "__main__":
    main()
