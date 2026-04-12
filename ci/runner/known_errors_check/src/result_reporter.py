"""
JSON report builder, Markdown table formatter, and console summary printer
for known_errors_check.

Report schema:
{
  "<error_id>": {
    "<example_name>-<type>": true   // true = known error reproduced, false = not found
  }
}
"""

import json
import sys

from common.markdown_table import build_markdown_table
from known_errors_check.src.error_checker import ErrorResult


def build_report(error_results: list[ErrorResult]) -> dict:
    report: dict[str, dict[str, bool]] = {}
    for er in error_results:
        examples: dict[str, bool] = {}
        for example in er.examples:
            examples[f"{example.example_name}-{example.example_type}"] = example.reproduced
        report[er.error_id] = examples
    return report


def save_report(report: dict, output_path: str) -> None:
    with open(output_path, "w", encoding="utf-8") as f:
        json.dump(report, f, indent=2)


def format_markdown_table(
    report: dict[str, dict[str, bool]],
    error_url_prefix: str | None = None,
) -> str:
    """
    Build a Markdown table from a known-errors report.

    Each row represents one (error_id, example_name-type) pair.
    Error IDs are rendered as links when error_url_prefix is provided.
    """
    rows = []
    for error_id in sorted(report):
        id_cell = f"[{error_id}]({error_url_prefix}/{error_id})" if error_url_prefix else error_id
        for example_key in sorted(report[error_id]):
            reproduced = report[error_id][example_key]
            rows.append([id_cell, example_key, "✅" if reproduced else "❌"])

    if not rows:
        return "No known errors checked."

    return build_markdown_table(
        headers=["Error ID", "Example", "Reproduced"],
        rows=rows,
        title="Known Errors Regression Check",
    )


def print_summary(
    error_results: list[ErrorResult],
    new_error_incidents: list[dict],
    regression_confirmations: list[dict],
) -> None:
    """Print a human-readable summary to stderr."""
    sep = "\n" + "=" * 80

    lines = [sep, "Known Errors Regression Check", sep]

    # --- Own-errors regression section ---
    if regression_confirmations:
        lines.append("Regression check for own known errors:")
        for reg in regression_confirmations:
            mark = "· still reproduces" if reg["reproduced"] else "✓ NOT reproducing (bug may be fixed!)"
            lines.append(f"  {reg['error_id']} / {reg['example_name']} ({reg['example_type']}): {mark}")
    else:
        lines.append("No own known errors to check regression for.")

    # --- Unknown errors section ---
    lines.append(sep)
    if new_error_incidents:
        lines.append(f"UNKNOWN ERRORS FOUND ({len(new_error_incidents)} total) — CI will fail:")
        for inc in new_error_incidents:
            lines.append(
                f"  error_id={inc['error_id']} " f"originating={inc['originating_tool']} " f"example={inc['example_name']} ({inc['example_type']})"
            )
            if inc.get("output_excerpt"):
                lines.append(f"    output: {inc['output_excerpt'][:200]}")
    else:
        lines.append("No unknown errors found across all known error examples.")

    lines.append(sep)
    print("\n".join(lines), file=sys.stderr)
