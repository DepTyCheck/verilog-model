#!/usr/bin/env python3

import json
import logging
import sys
from pathlib import Path

from common.logger import configure_logger
from common.tool_error_regex import ToolErrorRegex
from tools_run.src.assets import Assets
from tools_run.src.ignored_errors_list import IgnoredErrorsList
from tools_run.src.known_errors_report import KnownErrorsReport
from tools_run.src.mds_distances_report import MDSDistancesReport
from tools_run.src.parse_args import parse_args
from tools_run.src.print_stats import print_failed_tests_paths, print_issues_count, print_unexpected_errors
from tools_run.src.tests_list import CommandConfig, TestsList
from tools_run.src.utils import print_pretty


def _parse_commands(commands_json: str) -> list[CommandConfig]:
    raw = json.loads(commands_json)
    result = []
    for entry in raw:
        regex_str = entry.get("error_regex")
        result.append(
            CommandConfig(
                run=entry["run"],
                error_regex=ToolErrorRegex(raw_str_regex=regex_str) if regex_str else None,
            )
        )
    return result


def main() -> None:
    configure_logger(level=logging.DEBUG)

    args = parse_args()

    ignored_errors = IgnoredErrorsList(
        args.ignored_errors_dir,
        args.tool_name,
        regex_list=args.extra_ignored_regexes,
    )

    commands = _parse_commands(args.commands_json)

    tests_result = TestsList(
        files=Path(args.gen_path).glob(args.file_pattern),
        ignored_errors_list=ignored_errors,
        commands=commands,
        assets=Assets(args.assets),
    ).run_all()

    known_errors_report = KnownErrorsReport(commit=args.commit)
    known_errors_report.add_errors(tests_result.matches)
    known_errors_report.save(args.run_statistics_output)

    print_issues_count(tests_result)

    if tests_result.has_unexpected_errors():
        print_unexpected_errors(tests_result)
        print_failed_tests_paths(tests_result)

        MDSDistancesReport(
            new_errors=tests_result.unexpected_errors,
            ignored_errors=ignored_errors,
            tool_name=args.tool_name,
            job_link=args.job_link,
        ).save(args.error_distances_output)

        sys.exit(1)
    else:
        print_pretty(["  All tests passed successfully."])


if __name__ == "__main__":
    main()
