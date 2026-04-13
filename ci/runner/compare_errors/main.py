import argparse
import json
from pathlib import Path

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
    parser.add_argument(
        "--error-url-prefix",
        type=str,
        required=False,
        default=None,
        help="URL prefix for error links, e.g. https://org.github.io/repo/error (error ID will be appended)",
    )
    parser.add_argument(
        "--known-errors-reports-dir",
        type=str,
        required=False,
        default=None,
        help=(
            "Directory containing regression-test JSON reports "
            "(schema: {error_id: {example_name: bool}}). "
            "When provided, a 'Reproduced' column is added to the table."
        ),
    )
    return parser.parse_args()


def _load_known_errors(reports_dir: str) -> dict[str, bool]:
    """
    Scan reports_dir for *.json files and merge them into a flat
    {error_id: reproduced} dict.  An error is reproduced if any example
    in any report file evaluates to True.
    """
    result: dict[str, bool] = {}
    for path in Path(reports_dir).glob("*.json"):
        try:
            with open(path, encoding="utf-8") as f:
                report: dict[str, dict[str, bool]] = json.load(f)
            for error_id, examples in report.items():
                result[error_id] = result.get(error_id, False) or any(examples.values())
        except Exception:
            pass
    return result


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

    known_errors = _load_known_errors(args.known_errors_reports_dir) if args.known_errors_reports_dir else None

    deltas = comparison.compare()
    table = format_table(deltas, error_url_prefix=args.error_url_prefix, known_errors=known_errors)
    print(table)


if __name__ == "__main__":
    main()
