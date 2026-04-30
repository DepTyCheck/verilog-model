import argparse

from combined_report.previous_report import PreviousReport
from legacy_stats.first_found_index import FirstFoundIndex
from legacy_stats.legacy_report import LegacyReport


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate legacy_stats.csv mirroring website bugs-table fields.")
    parser.add_argument("--previous-report", type=str, required=True, help="Path to latest-error-stats.json")
    parser.add_argument(
        "--found-errors-dir",
        type=str,
        required=True,
        help="Path to verilog-gh-pages/found_errors directory containing tool subdirs of yaml files",
    )
    parser.add_argument("--output", type=str, required=True, help="Path to write legacy_stats.csv")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    prev = PreviousReport(args.previous_report)
    idx = FirstFoundIndex(args.found_errors_dir)
    LegacyReport.build(prev, idx).save_csv(args.output)


if __name__ == "__main__":
    main()
