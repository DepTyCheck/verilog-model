import json
from dataclasses import dataclass, field

from common.command_output import AnalyzisResult
from common.error_types import KnownError


@dataclass
class MatchRecord:
    error_id: str
    matched_text: str


@dataclass
class FileRecord:
    filename: str
    outcome: str
    matches: list[MatchRecord] = field(default_factory=list)


class PerFileReport:
    def __init__(
        self,
        tool_name: str,
        tool_version: str,
        tool_commit: str,
        model_commit: str,
        run_date: str,
    ) -> None:
        self.tool_name = tool_name
        self.tool_version = tool_version
        self.tool_commit = tool_commit
        self.model_commit = model_commit
        self.run_date = run_date
        self._files: list[FileRecord] = []

    def add_result(self, filename: str, result: AnalyzisResult) -> None:
        outcome = _get_outcome(result)
        matches = [
            MatchRecord(error_id=em.match.error.error_id, matched_text=em.match.matched_text)
            for em in result.found_matches
            if isinstance(em.match.error, KnownError)
        ]
        self._files.append(FileRecord(filename=filename, outcome=outcome, matches=matches))

    def save(self, path: str) -> None:
        data = {
            "tool_name": self.tool_name,
            "tool_version": self.tool_version,
            "tool_commit": self.tool_commit,
            "model_commit": self.model_commit,
            "run_date": self.run_date,
            "files": [
                {
                    "filename": f.filename,
                    "outcome": f.outcome,
                    "matches": [{"error_id": m.error_id, "matched_text": m.matched_text} for m in f.matches],
                }
                for f in self._files
            ],
        }
        with open(path, "w", encoding="utf-8") as fh:
            json.dump(data, fh, indent=2)


def _get_outcome(result: AnalyzisResult) -> str:
    if result.timed_out:
        return "timeout"
    if result.unexpected_errors:
        return "unknown"
    if result.found_matches:
        return "known_errors"
    return "clean"
