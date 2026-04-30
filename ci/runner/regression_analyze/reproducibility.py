# ci/runner/regression_analyze/reproducibility.py
"""
Pure logic for building the reproducibility table from a PerFileReport plus
a {(<example_name>, <type>): error_id} expectation map.
"""

from pathlib import Path

from common.markdown_table import build_markdown_table
from common.per_file_report import PerFileReport

_TYPES = ("minified", "full")


def parse_example_filename(filename: str) -> tuple[str, str] | None:
    """
    Split <example_name>-<type>.<ext> into (example_name, type).

    Splits on the LAST hyphen in the stem, so example names that contain
    hyphens themselves are preserved. Returns None if the trailing token is
    not "minified" or "full".
    """
    stem = Path(filename).stem
    if "-" not in stem:
        return None
    name, _, ttype = stem.rpartition("-")
    if ttype not in _TYPES:
        return None
    return name, ttype


def build_reproducibility_table(
    report: PerFileReport,
    expected: dict[tuple[str, str], str],
) -> dict[str, dict[str, bool]]:
    """
    expected: maps (example_name, type) -> error_id.

    Returns: {error_id: {<example_name>-<type>: reproduced_bool}}.

    Files in the report whose (name, type) is not in `expected` are skipped
    (e.g. cross-language examples or stale entries).
    """
    table: dict[str, dict[str, bool]] = {}
    for f in report.files:
        parsed = parse_example_filename(f.filename)
        if parsed is None:
            continue
        if parsed not in expected:
            continue
        error_id = expected[parsed]
        reproduced = any(m.error_id == error_id for c in f.commands for m in c.matches)
        key = f"{parsed[0]}-{parsed[1]}"
        table.setdefault(error_id, {})[key] = reproduced
    return table


def format_markdown_table(
    table: dict[str, dict[str, bool]],
    error_url_prefix: str | None = None,
) -> str:
    rows: list[list[str]] = []
    for error_id in sorted(table):
        id_cell = f"[{error_id}]({error_url_prefix}/{error_id})" if error_url_prefix else error_id
        for key in sorted(table[error_id]):
            rows.append([id_cell, key, "✅" if table[error_id][key] else "❌"])
    if not rows:
        return "No known errors checked."
    return build_markdown_table(
        headers=["Error ID", "Example", "Reproduced"],
        rows=rows,
        title="Known Errors Regression Check",
    )
