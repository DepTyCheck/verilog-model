import json
from dataclasses import dataclass
from pathlib import Path


@dataclass
class MatchRecord:
    error_id: str
    matched_text: str


@dataclass
class FileRecord:
    filename: str
    outcome: str
    matches: list[MatchRecord]


@dataclass
class ToolReport:
    tool_name: str
    tool_version: str
    tool_commit: str
    model_commit: str
    run_date: str
    files: list[FileRecord]


def load_report(path: Path) -> ToolReport:
    data = json.loads(path.read_text(encoding="utf-8"))
    files = [
        FileRecord(
            filename=f["filename"],
            outcome=f["outcome"],
            matches=[MatchRecord(**m) for m in f.get("matches", [])],
        )
        for f in data["files"]
    ]
    return ToolReport(
        tool_name=data["tool_name"],
        tool_version=data["tool_version"],
        tool_commit=data["tool_commit"],
        model_commit=data["model_commit"],
        run_date=data["run_date"],
        files=files,
    )
