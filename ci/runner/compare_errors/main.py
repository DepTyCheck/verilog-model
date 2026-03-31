import argparse

from combined_report.previous_report import PreviousReport
from combined_report.tools_report_list import ToolsReportsList

from .compare_errors import ErrorsComparison
from .table_formatter import format_table


def parse_args():
    parser = argparse.ArgumentParser(description="Compare error statistics between historical data and current run")
    parser.add_argument(
        "--previous-report",
        type=str,
        required=True,
        help="Path to the previous combined report JSON (historical data)",
    )
    parser.add_argument(
        "--current-tools-reports-dir",
        type=str,
        required=True,
        help="Directory containing current run tool report JSON files",
    )
    parser.add_argument(
        "--tools-reports-pattern",
        type=str,
        required=True,
        help="Glob pattern to match tool report files",
    )
    parser.add_argument(
        "--tests-number",
        type=int,
        required=True,
        help="Number of test cases in the current run",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()

    previous_report = PreviousReport(file_path=args.previous_report)
    tools_reports_list = ToolsReportsList(
        dir_path=args.current_tools_reports_dir,
        pattern=args.tools_reports_pattern,
    )

    comparison = ErrorsComparison(
        previous_report=previous_report,
        tools_reports_list=tools_reports_list,
        tests_number=args.tests_number,
    )

    deltas = comparison.compare()
    table = format_table(deltas)
    print(table)


if __name__ == "__main__":
    main()
