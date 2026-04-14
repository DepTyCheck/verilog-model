#!/usr/bin/env python3
"""
mds_report — generate an MDS distance diagram from unknown errors.

Usage (from ci/runner/):
  python -m mds_report.main \
    --unknown-errors-input unknown_errors.json \
    --ignored-errors-dir   verilog-gh-pages/found_errors/iverilog \
    --tool-name            iverilog \
    --job-link             https://github.com/... \
    --output               error_distances.html
"""

import json
import logging

from common.logger import configure_logger
from common.unknown_error_reporter import UnknownErrorEntry
from mds_report.mds_distances_report import MDSDistancesReport
from mds_report.parse_args import parse_args
from tools_run.src.ignored_errors_list import IgnoredErrorsList


def main() -> None:
    configure_logger(level=logging.INFO)
    args = parse_args()

    with open(args.unknown_errors_input, encoding="utf-8") as fh:
        raw = json.load(fh)
    new_errors = [UnknownErrorEntry(**entry) for entry in raw]

    ignored_errors = IgnoredErrorsList(
        dir_path=args.ignored_errors_dir,
        tool=args.tool_name,
    )

    MDSDistancesReport(
        new_errors=new_errors,
        ignored_errors=ignored_errors,
        tool_name=args.tool_name,
        job_link=args.job_link,
    ).save(args.output)


if __name__ == "__main__":
    main()
