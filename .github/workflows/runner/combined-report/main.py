import sys
import argparse
from pathlib import Path


from src.previous_report import PreviousReport
from src.tools_report_list import ToolsReportsList
from src.combined_report import CombinedReport
from src.result_report import ResultReport


def parse_args():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--previous-report",
        type=str,
        help="Path to previous report",
        required=True,
    )
    parser.add_argument(
        "--current-tools-reports-dir",
        type=str,
        help="Path to tools reports directory",
        required=True,
    )
    parser.add_argument(
        "--tools-reports-pattern",
        type=str,
        help="Tools reports pattern",
        required=True,
    )
    parser.add_argument(
        "--tests-number",
        type=int,
        help="Number of tests",
        required=True,
    )
    parser.add_argument(
        "--result-report",
        type=str,
        help="Path to result report",
        required=True,
    )

    return parser.parse_args()


def main() -> None:
    args = parse_args()

    CombinedReport(
        previous_report=PreviousReport(
            file_path=args.previous_report,
        ),
        tools_reports_list=ToolsReportsList(
            dir_path=args.current_tools_reports_dir,
            pattern=args.tools_reports_pattern,
        ),
        tests_number=args.tests_number,
    ).combine().save(args.result_report)


if __name__ == "__main__":
    main()
