import argparse
from pathlib import Path

from .count_lines import count_lines, format_report


def parse_args():
    parser = argparse.ArgumentParser(description="Count lines of code in .idr specification files")
    parser.add_argument(
        "src_dir",
        type=str,
        help="Root directory to scan for .idr files",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    counts = count_lines(Path(args.src_dir))
    print(format_report(counts))


if __name__ == "__main__":
    main()
