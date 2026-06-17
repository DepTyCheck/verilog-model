import argparse
from datetime import date, datetime
from pathlib import Path

from common.argparse_helpers import add_error_url_prefix_arg
from common.first_found_index import FirstFoundIndex
from dataset_stats.combined_report import CombinedReport
from dataset_stats.files_index import FilesIndex
from dataset_stats.issues_index import IssuesIndex
from dataset_stats.legacy_index import LegacyIndex

from .compare_errors import ErrorsComparison
from .current_index import CurrentIndex
from .historical_index import HistoricalIndex
from .master_index import MasterIndex
from .reproduced_index import ReproducedIndex
from .table_formatter import format_table


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description="Compare per-error_id occurrence percentages between the dataset branch and the current PR.",
    )
    p.add_argument("--dataset-path", required=True, help="Path to a clone of the dataset branch")
    p.add_argument("--per-file-results-dir", required=True, help="Directory of <tool>-per-file.json artifacts from run-tools")
    p.add_argument("--regression-results-dir", required=True, help="Directory of regression-<tool>-per-file.json artifacts from regression-test")
    p.add_argument(
        "--master-commit-date",
        required=True,
        help="Date (YYYY-MM-DD) of the last commit on master; lower bound of the master-frequency window",
    )
    add_error_url_prefix_arg(p, "URL prefix for error links, e.g. https://org.github.io/repo/error (error_id will be appended)")
    return p.parse_args()


def main() -> None:
    args = parse_args()

    root = Path(args.dataset_path)
    issues = IssuesIndex(root / "issues.csv")
    files = FilesIndex(root / "files")
    first_found = FirstFoundIndex(root / "found_issues")
    legacy = LegacyIndex(root / "legacy_stats.csv")

    report = CombinedReport.build(issues=issues, files=files, first_found=first_found, legacy=legacy)
    historical = HistoricalIndex(report)

    master_commit_date = datetime.strptime(args.master_commit_date, "%Y-%m-%d").date()
    master = MasterIndex(
        issues=issues,
        files=files,
        first_found=first_found,
        master_commit_date=master_commit_date,
        today=date.today(),
    )

    current = CurrentIndex(args.per_file_results_dir)
    reproduced = ReproducedIndex(args.regression_results_dir, args.dataset_path)

    deltas = ErrorsComparison(historical=historical, current=current, master=master).compare()
    known_errors = {d.error_id: reproduced.reproduced(d.error_id) for d in deltas}

    print(format_table(deltas, error_url_prefix=args.error_url_prefix, known_errors=known_errors))


if __name__ == "__main__":
    main()
