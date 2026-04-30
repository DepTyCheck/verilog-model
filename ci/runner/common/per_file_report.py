# ci/runner/common/per_file_report.py
"""
Canonical per-file JSON schema, writer, loader, and outcome helper.

Schema (top level):
{
  "tool_name", "tool_version", "tool_commit", "model_commit", "run_date",
  "files": [
    {
      "filename": str,
      "commands": [
        {
          "command": str,
          "outcome": "clean" | "known_errors" | "unknown" | "timeout",
          "matches": [{"error_id": str, "matched_text": str}, ...]
        },
        ...
      ]
    },
    ...
  ]
}

File-level outcome is NOT stored. Consumers derive it via `file_outcome()`.

The string `"unknown"` is reserved as a sentinel `error_id`. No registered
KnownError uses it.
"""

import json
from dataclasses import dataclass, field
from pathlib import Path


@dataclass
class MatchRecord:
    error_id: str
    matched_text: str


@dataclass
class CommandRecord:
    command: str
    outcome: str
    matches: list[MatchRecord] = field(default_factory=list)


@dataclass
class FileRecord:
    filename: str
    commands: list[CommandRecord] = field(default_factory=list)


@dataclass
class PerFileReport:
    tool_name: str
    tool_version: str
    tool_commit: str
    model_commit: str
    run_date: str
    files: list[FileRecord] = field(default_factory=list)

    def add_file(self, record: FileRecord) -> None:
        self.files.append(record)

    def save(self, path: str | Path) -> None:
        data = {
            "tool_name": self.tool_name,
            "tool_version": self.tool_version,
            "tool_commit": self.tool_commit,
            "model_commit": self.model_commit,
            "run_date": self.run_date,
            "files": [
                {
                    "filename": f.filename,
                    "commands": [
                        {
                            "command": c.command,
                            "outcome": c.outcome,
                            "matches": [{"error_id": m.error_id, "matched_text": m.matched_text} for m in c.matches],
                        }
                        for c in f.commands
                    ],
                }
                for f in self.files
            ],
        }
        with open(path, "w", encoding="utf-8") as fh:
            json.dump(data, fh, indent=2)


def load_report(path: str | Path) -> PerFileReport:
    """Load a per-file report previously written by ``PerFileReport.save``.

    Caller must ensure the file conforms to the canonical schema. Malformed
    input raises ``KeyError`` (missing key) or ``json.JSONDecodeError`` (bad
    JSON); the loader does no validation beyond the natural attribute access.
    """
    with open(path, encoding="utf-8") as fh:
        data = json.load(fh)
    files = [
        FileRecord(
            filename=f["filename"],
            commands=[
                CommandRecord(
                    command=c["command"],
                    outcome=c["outcome"],
                    matches=[MatchRecord(error_id=m["error_id"], matched_text=m["matched_text"]) for m in c.get("matches", [])],
                )
                for c in f.get("commands", [])
            ],
        )
        for f in data["files"]
    ]
    return PerFileReport(
        tool_name=data["tool_name"],
        tool_version=data["tool_version"],
        tool_commit=data["tool_commit"],
        model_commit=data["model_commit"],
        run_date=data["run_date"],
        files=files,
    )


_OUTCOME_PRECEDENCE = ("timeout", "unknown", "known_errors", "clean")


def file_outcome(record: FileRecord) -> str:
    """Derive a file-level outcome from per-command outcomes.

    Precedence (highest to lowest): timeout > unknown > known_errors > clean.
    Empty commands list returns "clean".
    """
    outcomes = {c.outcome for c in record.commands}
    for level in _OUTCOME_PRECEDENCE:
        if level in outcomes:
            return level
    return "clean"
