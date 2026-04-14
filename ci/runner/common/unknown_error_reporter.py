import json
from dataclasses import asdict, dataclass

from common.command_output import AnalyzisResult
from common.tool_matrix_runner import FileInput


@dataclass
class UnknownErrorEntry:
    file_path: str
    error_text: str


def collect_unknown_errors(
    results: list[tuple[FileInput, AnalyzisResult]],
) -> list[UnknownErrorEntry]:
    """Return one UnknownErrorEntry per unexpected error across all results."""
    entries: list[UnknownErrorEntry] = []
    for _, result in results:
        for err in result.unexpected_errors:
            entries.append(
                UnknownErrorEntry(
                    file_path=err.test_file_path,
                    error_text=err.tool_output_error_text,
                )
            )
    return entries


def print_unknown_errors(entries: list[UnknownErrorEntry]) -> None:
    """Print file path and error text for each unknown error. Nothing if empty."""
    if not entries:
        return
    lines = [f"Unknown errors found ({len(entries)} total):"]
    for i, e in enumerate(entries, 1):
        lines.append(f"  [{i}] {e.file_path}")
        lines.append(f"      {e.error_text}")
    print("\n".join(lines))


def save_unknown_errors_json(entries: list[UnknownErrorEntry], path: str) -> None:
    """Write entries to a JSON file consumable by mds_report/main.py."""
    with open(path, "w", encoding="utf-8") as fh:
        json.dump([asdict(e) for e in entries], fh, indent=2)
