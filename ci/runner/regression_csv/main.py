#!/usr/bin/env python3
"""Build latest_regression_test.csv from regression per-file artifacts.

Inputs:
  --regression-results-dir : dir of regression-<tool>-per-file.json files.
  --known-errors-dir       : root of found_issues/ on the dataset branch clone.
  --out-csv                : destination path; overwritten if rows exist.

Behaviour:
  - Globs `regression-*-per-file.json`. Zero artifacts → log warning, exit 0,
    no file written (previous CSV stays as-is on the dataset branch).
  - Per artifact: derive tool name from filename, build expected map, collect
    `(example_id, type, reproduced)` rows.
  - Aggregate, sort by `(example_id, type)`, write CSV.
  - Zero rows after aggregation → log warning, exit 0, no file written.
"""

import logging
from pathlib import Path

from common.logger import configure_logger, get_logger
from regression_analyze.expected import build_expected
from regression_csv.csv_writer import format_csv
from regression_csv.extract import extract_rows_from_report, tool_name_from_artifact
from regression_csv.parse_args import parse_args


def _collect_rows(regression_dir: Path, known_errors_dir: str) -> list[tuple[str, str, bool]]:
    logger = get_logger()
    rows: list[tuple[str, str, bool]] = []
    if not regression_dir.exists():
        logger.warning(f"Regression results dir does not exist: {regression_dir}")
        return rows
    artifacts = sorted(regression_dir.glob("regression-*-per-file.json"))
    if not artifacts:
        logger.warning(f"No regression artifacts in {regression_dir}")
        return rows
    for artifact in artifacts:
        # Defensive: glob "regression-*-per-file.json" can match degenerate
        # names like "regression--per-file.json" that the stricter regex rejects.
        tool = tool_name_from_artifact(artifact.name)
        if tool is None:
            logger.warning(f"Skipping artifact with unrecognised filename: {artifact.name}")
            continue
        expected = build_expected(known_errors_dir, tool)
        if not expected:
            logger.info(f"No known errors registered for tool {tool!r}; 0 rows from {artifact.name}")
            continue
        rows.extend(extract_rows_from_report(artifact, expected))
    return rows


def main() -> None:
    configure_logger(level=logging.INFO)
    args = parse_args()
    rows = _collect_rows(Path(args.regression_results_dir), args.known_errors_dir)
    if not rows:
        get_logger().warning("No regression rows produced; latest_regression_test.csv not written")
        return
    rows.sort(key=lambda r: (r[0], r[1]))
    Path(args.out_csv).write_text(format_csv(rows), encoding="utf-8")
    get_logger().info(f"Wrote {len(rows)} rows to {args.out_csv}")


if __name__ == "__main__":
    main()
