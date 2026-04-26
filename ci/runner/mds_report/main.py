# ci/runner/mds_report/main.py
"""
mds_report — generate an MDS distance diagram from a per-file JSON report.

Reads a runner.main per-file JSON, flattens commands[].matches[], collects
matches whose error_id == "unknown", and plots them against the registered
known errors for the tool.
"""

import logging

from common.ignored_errors_list import IgnoredErrorsList
from common.logger import configure_logger
from common.per_file_report import load_report
from mds_report.mds_distances_report import MDSDistancesReport
from mds_report.parse_args import parse_args
from mds_report.unknown_error_entry import UnknownErrorEntry


def _collect_unknowns(report) -> list[UnknownErrorEntry]:
    entries: list[UnknownErrorEntry] = []
    for f in report.files:
        for c in f.commands:
            for m in c.matches:
                if m.error_id == "unknown":
                    entries.append(UnknownErrorEntry(file_path=f.filename, error_text=m.matched_text))
    return entries


def main() -> None:
    configure_logger(level=logging.INFO)
    args = parse_args()

    report = load_report(args.per_file_input)
    new_errors = _collect_unknowns(report)

    ignored_errors = IgnoredErrorsList(dir_path=args.ignored_errors_dir, tool=args.tool_name)

    MDSDistancesReport(
        new_errors=new_errors,
        ignored_errors=ignored_errors,
        tool_name=args.tool_name,
        job_link=args.job_link,
    ).save(args.output)


if __name__ == "__main__":
    main()
