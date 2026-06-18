"""Shared fixture builders for compare_errors tests."""

import json
import os
import textwrap
from pathlib import Path

# Reuses ci/runner/dataset_stats/tests/data/ — see plan
# docs/superpowers/plans/2026-04-30-compare-errors-revival.md Task 2.
DATASET_DATA_DIR = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    "..",
    "..",
    "dataset_stats",
    "tests",
    "data",
)
ISSUES = os.path.join(DATASET_DATA_DIR, "issues.csv")
FILES = os.path.join(DATASET_DATA_DIR, "files")
FOUND = os.path.join(DATASET_DATA_DIR, "found_issues")
LEGACY = os.path.join(DATASET_DATA_DIR, "legacy_stats.csv")


def write_per_file(
    dir_path: Path | str,
    tool_name: str,
    files: list,
    *,
    suffix: str = "",
    name_prefix: str = "",
) -> Path:
    """Write a per-file JSON payload for ``tool_name`` to ``dir_path``.

    ``suffix`` lets a caller distinguish multiple JSONs for the same tool.
    ``name_prefix`` is prepended to the filename (e.g. ``"regression-"``).
    """
    payload = {
        "tool_name": tool_name,
        "tool_version": "v",
        "tool_commit": "c",
        "model_commit": "m",
        "run_date": "2026-04-30",
        "files": files,
    }
    out = Path(dir_path) / f"{name_prefix}{tool_name}{suffix}-per-file.json"
    out.write_text(json.dumps(payload), encoding="utf-8")
    return out


def make_file(filename: str, *match_pairs: tuple[str, str]) -> dict:
    """Build a per-file file entry. Empty ``match_pairs`` => clean outcome."""
    return {
        "filename": filename,
        "commands": [
            {
                "command": "tool args",
                "outcome": "known_errors" if match_pairs else "clean",
                "matches": [{"error_id": eid, "matched_text": text} for eid, text in match_pairs],
            }
        ],
    }


def write_yaml(
    dataset: Path,
    tool: str,
    error_id: str,
    language: str,
    examples: list[tuple[str, str]],
) -> None:
    """Write a found_issues yaml under ``dataset/found_issues/<tool>/``."""
    yaml_dir = dataset / "found_issues" / tool
    yaml_dir.mkdir(parents=True, exist_ok=True)
    blocks = []
    for name, kind in examples:
        blocks.append(f"  - {name}:\n      {kind}_example: |\n        // body\n      first_found: 01.01.2026")
    body = textwrap.dedent(f"""
        id: {error_id}
        target: {tool}
        regex: "boom"
        title: "title"
        profile: {language}
        examples:
        """).strip() + "\n" + "\n".join(blocks) + "\n"
    (yaml_dir / f"{error_id}.yaml").write_text(body, encoding="utf-8")
