# ci/runner/runner/main.py
import logging
import sys
from datetime import datetime, timezone
from pathlib import Path

from common.assets import Assets
from common.command_config import parse_commands
from common.ignored_errors_list import IgnoredErrorsList
from common.input_iter import iter_test_files
from common.logger import configure_logger, get_logger
from common.per_file_report import CommandRecord, FileRecord, MatchRecord, PerFileReport
from common.tool_matrix_runner import ResultCollector, run_all
from runner.parse_args import parse_args


def main() -> None:
    configure_logger(level=logging.DEBUG)
    args = parse_args()
    logger = get_logger()

    known_errors = IgnoredErrorsList(
        args.ignored_errors_dir,
        args.tool_name,
        regex_list=args.extra_ignored_regexes,
    )
    commands = parse_commands(args.commands_json)
    assets = Assets(args.assets) if args.assets else None
    file_suffix = Path(args.file_pattern).suffix or ".sv"

    input_dir = Path(args.input_dir)
    if not input_dir.exists():
        logger.warning(f"input-dir does not exist: {input_dir.resolve()}")
        files: list[Path] = []
    else:
        files = sorted(input_dir.glob(args.file_pattern))
    if input_dir.exists() and not files:
        logger.warning(f"No files found matching {args.file_pattern!r} in {input_dir.resolve()}")
    logger.info(f"Found {len(files)} files matching {args.file_pattern!r} in {input_dir}")

    collector = ResultCollector()
    run_all(iter_test_files(files, file_suffix, assets), commands, known_errors, collector)

    report = PerFileReport(
        tool_name=args.tool_name,
        tool_version=args.tool_version,
        tool_commit=args.tool_commit,
        model_commit=args.model_commit,
        run_date=datetime.now(timezone.utc).strftime("%Y-%m-%d"),
    )
    for file_input, command_results in collector.results():
        report.add_file(
            FileRecord(
                filename=Path(file_input.context).name,
                commands=[
                    CommandRecord(
                        command=cr.command,
                        outcome=cr.outcome,
                        matches=[MatchRecord(error_id=m.error_id, matched_text=m.matched_text) for m in cr.matches],
                    )
                    for cr in command_results
                ],
            )
        )

    report.save(args.output)
    logger.info(f"Per-file report saved to {args.output}")

    has_unknown = collector.has_unknown_errors()
    sys.exit(1 if has_unknown else 0)


if __name__ == "__main__":
    main()
