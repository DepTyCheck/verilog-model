"""
JSON report builder, Markdown table formatter, and reproducibility report builder
for regression_test.

Report schema:
{
  "<error_id>": {
    "<example_name>-<type>": true   // true = known error reproduced, false = not found
  }
}
"""

import json

from common.command_output import AnalyzisResult
from common.error_types import KnownError
from common.markdown_table import build_markdown_table
from common.tool_matrix_runner import FileInput


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


def build_reproducibility_report(
    results: list[tuple[FileInput, AnalyzisResult]],
    tool_name: str,
) -> dict[str, dict[str, bool]]:
    """
    Build the per-error-id reproducibility dict from collector results.

    Only processes results whose context is (ErrorFile, Example) and whose
    error_file.tool matches tool_name.

    Returns: { error_id: { "example_name-type": bool } }
    """
    report: dict[str, dict[str, bool]] = {}
    for file_input, result in results:
        if not isinstance(file_input.context, tuple):
            continue
        error_file, example = file_input.context
        if error_file.tool != tool_name:
            continue

        reproduced = any(isinstance(m.match.error, KnownError) and m.match.error.error_id == error_file.error_id for m in result.found_matches)
        key = f"{example.name}-{example.type}"
        report.setdefault(error_file.error_id, {})[key] = reproduced

    return report
