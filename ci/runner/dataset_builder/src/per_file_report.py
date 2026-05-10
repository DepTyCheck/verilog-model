# ci/runner/dataset_builder/src/per_file_report.py
"""
Loader for the canonical per-file JSON. Exposes a flattened in-memory
view (file-level outcome + concatenated known-error matches) that matches
the shape `dataset_builder.main` and `dataset_builder.filter` already
expect, so they do not need to know about the per-command schema.
"""

from dataclasses import dataclass, field
from pathlib import Path

from common.per_file_report import MatchRecord, ToolMeta, file_outcome
from common.per_file_report import load_report as _load_canonical


@dataclass
class FileRecord:
    filename: str
    outcome: str
    matches: list[MatchRecord]


@dataclass
class ToolReport(ToolMeta):
    files: list[FileRecord] = field(default_factory=list)


def load_report(path: Path) -> ToolReport:
    canonical = _load_canonical(path)
    flat_files: list[FileRecord] = []
    for f in canonical.files:
        outcome = file_outcome(f)
        matches: list[MatchRecord] = []
        for c in f.commands:
            for m in c.matches:
                if m.error_id == "unknown":
                    continue
                matches.append(MatchRecord(error_id=m.error_id, matched_text=m.matched_text))
        flat_files.append(FileRecord(filename=f.filename, outcome=outcome, matches=matches))
    return ToolReport(
        tool_name=canonical.tool_name,
        tool_version=canonical.tool_version,
        tool_commit=canonical.tool_commit,
        model_commit=canonical.model_commit,
        run_date=canonical.run_date,
        files=flat_files,
    )
