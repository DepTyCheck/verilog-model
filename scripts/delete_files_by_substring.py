#!/usr/bin/env python3
"""Delete referenced files containing a substring and their CSV rows."""

from __future__ import annotations

import argparse
import csv
import os
import stat
import sys
import tempfile
from pathlib import Path


def file_contains(path: Path, needle: bytes, chunk_size: int = 1024 * 1024) -> bool:
    """Search a file as bytes without loading the whole file into memory."""
    overlap = b""
    with path.open("rb") as source:
        while chunk := source.read(chunk_size):
            data = overlap + chunk
            if needle in data:
                return True
            overlap = data[-(len(needle) - 1) :] if len(needle) > 1 else b""
    return False


def validate_inputs(substring: str, csv_path: Path, files_dir: Path) -> None:
    """Validate command inputs before doing any work."""
    if not substring:
        raise ValueError("substring must not be empty")
    if not csv_path.is_file():
        raise FileNotFoundError(f"CSV file not found: {csv_path}")
    if not files_dir.is_dir():
        raise NotADirectoryError(f"files directory not found: {files_dir}")


def read_referenced_names(csv_path: Path) -> set[str]:
    """Read the unique filenames from the last CSV column."""
    with csv_path.open("r", newline="", encoding="utf-8") as source:
        return {row[-1] for row in csv.reader(source) if row}


def find_matching_files(
    names: set[str], files_dir: Path, needle: bytes
) -> list[Path]:
    """Find referenced files whose contents contain the substring."""
    matching_files = []
    for name in names:
        if Path(name).name != name:
            raise ValueError(f"unsafe filename in CSV: {name!r}")

        path = files_dir / name
        if path.is_file() and file_contains(path, needle):
            matching_files.append(path)
    return matching_files


def count_referencing_rows(csv_path: Path, names: set[str]) -> int:
    """Count CSV rows that reference any of the given filenames."""
    with csv_path.open("r", newline="", encoding="utf-8") as source:
        return sum(1 for row in csv.reader(source) if row and row[-1] in names)


def write_filtered_csv(csv_path: Path, names: set[str]) -> tuple[Path, int]:
    """Write retained CSV rows to a temporary file."""
    temp_file = tempfile.NamedTemporaryFile(
        mode="w",
        newline="",
        encoding="utf-8",
        dir=csv_path.parent,
        prefix=f".{csv_path.name}.",
        suffix=".tmp",
        delete=False,
    )
    temp_path = Path(temp_file.name)
    removed_rows = 0

    try:
        with temp_file as target:
            os.fchmod(target.fileno(), stat.S_IMODE(csv_path.stat().st_mode))
            with csv_path.open("r", newline="", encoding="utf-8") as source:
                reader = csv.reader(source)
                writer = csv.writer(target, quoting=csv.QUOTE_ALL)
                for row in reader:
                    if row and row[-1] in names:
                        removed_rows += 1
                    else:
                        writer.writerow(row)
            target.flush()
            os.fsync(target.fileno())
    except Exception:
        temp_file.close()
        temp_path.unlink(missing_ok=True)
        raise

    return temp_path, removed_rows


def delete_files(paths: list[Path]) -> None:
    """Delete all matched files."""
    for path in paths:
        path.unlink()


def clean(
    substring: str,
    csv_path: Path,
    files_dir: Path,
    *,
    dry_run: bool = False,
) -> tuple[int, int]:
    """Delete matching files and CSV rows; return both counts."""
    validate_inputs(substring, csv_path, files_dir)
    referenced_names = read_referenced_names(csv_path)
    matching_files = find_matching_files(
        referenced_names, files_dir, substring.encode("utf-8")
    )
    matching_names = {path.name for path in matching_files}

    if not matching_files:
        return 0, 0
    if dry_run:
        removed_rows = count_referencing_rows(csv_path, matching_names)
        return len(matching_files), removed_rows

    temp_path: Path | None = None
    try:
        temp_path, removed_rows = write_filtered_csv(csv_path, matching_names)
        delete_files(matching_files)
        os.replace(temp_path, csv_path)
        temp_path = None
        return len(matching_files), removed_rows
    finally:
        if temp_path is not None:
            temp_path.unlink(missing_ok=True)


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("substring", help="literal substring to find in file contents")
    parser.add_argument("--csv", type=Path, default=Path("issues.csv"))
    parser.add_argument("--files-dir", type=Path, default=Path("files"))
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="report what would be removed without changing anything",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    try:
        file_count, row_count = clean(
            args.substring,
            args.csv,
            args.files_dir,
            dry_run=args.dry_run,
        )
    except (OSError, ValueError, csv.Error) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1

    if args.dry_run:
        print(
            f"Would delete {file_count} file(s) and remove {row_count} CSV row(s)."
        )
    else:
        print(f"Deleted {file_count} file(s) and removed {row_count} CSV row(s).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
