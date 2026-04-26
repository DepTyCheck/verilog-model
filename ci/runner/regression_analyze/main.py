# ci/runner/regression_analyze/main.py
import logging
from pathlib import Path

from common.error_file_parser import parse_error_files
from common.logger import configure_logger
from common.per_file_report import load_report
from regression_analyze.parse_args import parse_args
from regression_analyze.reproducibility import build_reproducibility_table, format_markdown_table


def _build_expected(known_errors_dir: str, tool_name: str, language: str) -> dict[tuple[str, str], str]:
    base = Path(known_errors_dir)
    expected: dict[tuple[str, str], str] = {}
    if not base.exists():
        return expected
    for sub in sorted(p for p in base.iterdir() if p.is_dir()):
        for ef in parse_error_files(str(sub)):
            if ef.tool != tool_name or ef.language != language:
                continue
            for ex in ef.examples:
                expected[(ex.name, ex.type)] = ef.error_id
    return expected


def main() -> None:
    configure_logger(level=logging.INFO)
    args = parse_args()

    expected = _build_expected(args.known_errors_dir, args.tool_name, args.language)
    report = load_report(args.per_file_input)
    table = build_reproducibility_table(report, expected)
    print(format_markdown_table(table, error_url_prefix=args.error_url_prefix))


if __name__ == "__main__":
    main()
