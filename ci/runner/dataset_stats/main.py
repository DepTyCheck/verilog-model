import argparse

from dataset_stats.combined_report import CombinedReport
from dataset_stats.files_index import FilesIndex
from dataset_stats.first_found_index import FirstFoundIndex
from dataset_stats.issues_index import IssuesIndex
from dataset_stats.legacy_index import LegacyIndex


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Build combined_stats.csv merging legacy_stats with issues.csv-derived stats.")
    p.add_argument("--issues-csv", required=True, help="Path to dataset/issues.csv")
    p.add_argument("--files-dir", required=True, help="Path to dataset/files directory")
    p.add_argument(
        "--found-issues-dir",
        required=True,
        help="Path to dataset/found_issues directory containing tool subdirs of yaml files",
    )
    p.add_argument(
        "--legacy-stats-csv",
        required=True,
        help="Path to dataset/legacy_stats.csv",
    )
    p.add_argument(
        "--output",
        required=True,
        help='Output CSV path. Use "-" to write to stdout.',
    )
    return p.parse_args()


def main() -> None:
    args = parse_args()
    report = CombinedReport.build(
        issues=IssuesIndex(args.issues_csv),
        files=FilesIndex(args.files_dir),
        first_found=FirstFoundIndex(args.found_issues_dir),
        legacy=LegacyIndex(args.legacy_stats_csv),
    )
    report.save_csv(args.output)


if __name__ == "__main__":
    main()
