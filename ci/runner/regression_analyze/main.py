# ci/runner/regression_analyze/main.py
import logging

from common.logger import configure_logger
from common.per_file_report import load_report
from regression_analyze.expected import build_expected
from regression_analyze.parse_args import parse_args
from regression_analyze.reproducibility import build_reproducibility_table, format_markdown_table


def main() -> None:
    configure_logger(level=logging.INFO)
    args = parse_args()

    expected = build_expected(args.known_errors_dir, args.tool_name)
    report = load_report(args.per_file_input)
    table = build_reproducibility_table(report, expected)
    print(format_markdown_table(table, error_url_prefix=args.error_url_prefix))


if __name__ == "__main__":
    main()
