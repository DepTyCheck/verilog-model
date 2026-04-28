#!/usr/bin/env python3

import argparse
import shutil
from pathlib import Path

from dataset_builder.src.csv_writer import append_rows
from dataset_builder.src.filter import passing_filenames
from dataset_builder.src.per_file_report import load_report
from dataset_builder.src.rename import rename_file


def _parse_args():
    p = argparse.ArgumentParser(description="Build dataset from per-file tool results.")
    p.add_argument(
        "--generated-dirs", nargs="+", required=True, help="Directories containing generated source files (e.g. generated-sv/ generated-vhdl/)"
    )
    p.add_argument("--per-file-results-dir", required=True, help="Directory containing *-per-file.json files from run-tools jobs")
    p.add_argument("--dataset-path", required=True, help="Path to cloned dataset branch")
    p.add_argument("--run-date", required=True, help="Date string in YYYY_MM_dd format (used in output filenames)")
    return p.parse_args()


def _find_source(filename: str, generated_dirs: list[Path]) -> Path:
    for d in generated_dirs:
        candidate = d / filename
        if candidate.exists():
            return candidate
    raise FileNotFoundError(f"Source file not found in any generated-dir: {filename!r}")


def _group_by_filename(reports, passing: set[str]) -> dict[str, list]:
    grouped: dict[str, list] = {fname: [] for fname in passing}
    for report in reports:
        for fr in report.files:
            if fr.filename in passing:
                grouped[fr.filename].append((report, fr))
    return grouped


def _match_rows(new_name: str, tool_records: list) -> list[dict]:
    rows: list[dict] = []
    for report, fr in tool_records:
        for match in fr.matches:
            rows.append(
                {
                    "filename": new_name,
                    "when_issue_occurred": report.run_date,
                    "tool_name": report.tool_name,
                    "error_id": match.error_id,
                    "tool_commit": report.tool_commit,
                    "model_commit": report.model_commit,
                }
            )
    return rows


def main() -> None:
    args = _parse_args()
    generated_dirs = [Path(d) for d in args.generated_dirs]
    dataset_path = Path(args.dataset_path)
    files_dir = dataset_path / "files"
    csv_path = dataset_path / "issues.csv"

    if not files_dir.is_dir():
        raise SystemExit(f"Dataset not initialised: {files_dir} does not exist. Create it on the dataset branch first.")
    if not csv_path.is_file():
        raise SystemExit(f"Dataset not initialised: {csv_path} does not exist. Create it on the dataset branch first.")

    per_file_dir = Path(args.per_file_results_dir)
    reports = [load_report(p) for p in sorted(per_file_dir.glob("*-per-file.json"))]
    file_tool_records = _group_by_filename(reports, passing_filenames(reports))

    rows: list[dict] = []
    files_added = 0
    for original_name in sorted(file_tool_records):
        new_name = rename_file(original_name, args.run_date)
        shutil.copy2(_find_source(original_name, generated_dirs), files_dir / new_name)
        files_added += 1
        rows.extend(_match_rows(new_name, file_tool_records[original_name]))

    append_rows(csv_path, rows)
    print(f"Dataset: {files_added} files added, {len(rows)} issue rows appended to {csv_path}.")


if __name__ == "__main__":
    main()
