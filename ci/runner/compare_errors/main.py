import argparse
from pathlib import Path

from common.argparse_helpers import add_error_url_prefix_arg
from dataset_stats.combined_report import CombinedReport
from dataset_stats.files_index import FilesIndex
from dataset_stats.first_found_index import FirstFoundIndex
from dataset_stats.issues_index import IssuesIndex
from dataset_stats.legacy_index import LegacyIndex

from .compare_errors import ErrorsComparison
from .current_index import CurrentIndex
from .historical_index import HistoricalIndex
from .reproduced_index import ReproducedIndex
from .table_formatter import format_table


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description="Compare per-error_id occurrence percentages between the dataset branch and the current PR.",
    )
    p.add_argument("--dataset-path", required=True, help="Path to a clone of the dataset branch")
    p.add_argument("--per-file-results-dir", required=True, help="Directory of <tool>-per-file.json artifacts from run-tools")
    p.add_argument("--regression-results-dir", required=True, help="Directory of regression-<tool>-per-file.json artifacts from regression-test")
    add_error_url_prefix_arg(p, "URL prefix for error links, e.g. https://org.github.io/repo/error (error_id will be appended)")
    return p.parse_args()


def _build_historical(dataset_path: str) -> HistoricalIndex:
    root = Path(dataset_path)
    report = CombinedReport.build(
        issues=IssuesIndex(root / "issues.csv"),
        files=FilesIndex(root / "files"),
        first_found=FirstFoundIndex(root / "found_issues"),
        legacy=LegacyIndex(root / "legacy_stats.csv"),
    )
    return HistoricalIndex(report)


def main() -> None:
    args = parse_args()

    historical = _build_historical(args.dataset_path)
    current = CurrentIndex(args.per_file_results_dir)
    reproduced = ReproducedIndex(args.regression_results_dir, args.dataset_path)

    deltas = ErrorsComparison(historical=historical, current=current).compare()
    known_errors = {d.error_id: reproduced.reproduced(d.error_id) for d in deltas}

    print(format_table(deltas, error_url_prefix=args.error_url_prefix, known_errors=known_errors))


if __name__ == "__main__":
    main()
