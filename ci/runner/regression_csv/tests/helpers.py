"""Shared fixture builders for regression_csv tests."""

import json
from pathlib import Path


def write_artifact(path: Path, tool: str, files: list[dict]) -> None:
    """Write a `regression-<tool>-per-file.json` artifact at `path`."""
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {
        "tool_name": tool,
        "tool_version": "v",
        "tool_commit": "c",
        "model_commit": "m",
        "run_date": "2026_05_15",
        "files": files,
    }
    path.write_text(json.dumps(payload), encoding="utf-8")


def clean_file(filename: str) -> dict:
    """A file entry whose single command produced a clean outcome."""
    return {
        "filename": filename,
        "commands": [{"command": "tool", "outcome": "clean", "matches": []}],
    }


def known_error_file(filename: str, error_id: str, matched_text: str) -> dict:
    """A file entry whose single command matched a known error."""
    return {
        "filename": filename,
        "commands": [
            {
                "command": "tool",
                "outcome": "known_errors",
                "matches": [{"error_id": error_id, "matched_text": matched_text}],
            }
        ],
    }
