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
from known_errors_check.src.error_checker import ErrorResult, ExampleStatus


def build_report(error_results: list[ErrorResult]) -> dict:
    report: dict[str, dict[str, bool]] = {}
    for er in error_results:
        examples: dict[str, bool] = {}
        for example in er.examples:
            found = any(r.status == ExampleStatus.KNOWN_ERROR for r in example.results_by_tool.values())
            examples[f"{example.example_name}-{example.example_type}"] = found
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

    Each row represents one (error_id, example_name) pair.
    Error IDs are rendered as links when error_url_prefix is provided.
    """
    rows = []
    for error_id in sorted(report):
        id_cell = f"[{error_id}]({error_url_prefix}/{error_id})" if error_url_prefix else error_id
        for example_name in sorted(report[error_id]):
            reproduced = report[error_id][example_name]
            rows.append([id_cell, example_name, "✅" if reproduced else "❌"])

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
        status_icons = {
            ExampleStatus.KNOWN_ERROR.value: "· still reproduces",
            ExampleStatus.CLEAN.value: "✓ CLEAN (bug may be fixed!)",
            ExampleStatus.NEW_ERROR.value: "✗ NEW ERROR",
            ExampleStatus.TIMEOUT.value: "⏱ TIMEOUT",
            ExampleStatus.EXEC_ERROR.value: "! EXEC ERROR",
        }
        for reg in regression_confirmations:
            icon = status_icons.get(reg["status"], reg["status"])
            lines.append(f"  [{reg['originating_tool']}] {reg['error_id']} / " f"{reg['example_name']} ({reg['example_type']}): {icon}")
    else:
        lines.append("No own known errors to check regression for.")

    # --- Cross-tool new errors section ---
    if new_error_incidents:
        lines.append(sep)
        lines.append(f"NEW ERRORS FOUND ({len(new_error_incidents)} total) — CI will fail:")
        for inc in new_error_incidents:
            lines.append(
                f"  error_id={inc['error_id']} "
                f"originating={inc['originating_tool']} "
                f"example={inc['example_name']} ({inc['example_type']}) "
                f"checked_with={inc['checked_with_tool']}"
            )
            if inc.get("output_excerpt"):
                lines.append(f"    output: {inc['output_excerpt'][:200]}")
    else:
        lines.append(sep)
        lines.append("No new errors found across all known error examples.")

    lines.append(sep)
    print("\n".join(lines), file=sys.stderr)
