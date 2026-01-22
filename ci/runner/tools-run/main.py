#!/usr/bin/env python3

import logging
import sys
from pathlib import Path

from src.assets import Assets
from src.ignored_errors_list import IgnoredErrorsList
from src.known_errors_report import KnownErrorsReport
from src.logger import configure_logger
from src.mds_distances_report import MDSDistancesReport
from src.parse_args import parse_args
from src.print_stats import print_failed_tests_paths, print_issues_count
from src.tests_list import TestsList
from src.tool_error_regex import ToolErrorRegex
from src.utils import print_pretty


def main() -> None:
    configure_logger(level=logging.DEBUG)

    args = parse_args()

    ignored_errors = IgnoredErrorsList(
        args.ignored_errors_dir,
        args.tool_name,
        regex_list=args.extra_ignored_regexes,
    )

    tests_result = TestsList(
        files=Path(args.gen_path).glob("*.sv"),
        ignored_errors_list=ignored_errors,
        main_error_regex=ToolErrorRegex(raw_str_regex=args.tool_error_regex),
        sim_error_regex=ToolErrorRegex(raw_str_regex=args.sim_error_regex) if args.sim_error_regex else None,
        raw_synth_cmd=args.tool_cmd,
        raw_sim_cmd=args.sim_cmd,
        assets=Assets(args.assets),
    ).run_all()

    KnownErrorsReport(commit=args.commit).save(args.run_statistics_output)

    print_issues_count(tests_result)

    if tests_result.has_unexpected_errors():
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
