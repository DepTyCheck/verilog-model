import csv
from dataclasses import dataclass
from datetime import date, datetime
from pathlib import Path


@dataclass(frozen=True)
class LegacyEntry:
    runs: int
    overall: int
    test_files: int
    last_tool_commit: str
    last_date: date


_REQUIRED_HEADER = [
    "error_id",
    "runs_for_that_issue",
    "overall_found_count",
    "test_files_count",
    "last_occurrence_tool_commit",
    "last_occurrence_date",
]


class LegacyIndex:
    def __init__(self, csv_path: str | Path):
        self._entries: dict[str, LegacyEntry] = {}
        self._build(Path(csv_path))

    def error_ids(self) -> list[str]:
        return list(self._entries)

    def lookup(self, error_id: str) -> LegacyEntry:
        if error_id not in self._entries:
            raise KeyError(f"error_id {error_id!r} not in legacy_stats.csv")
        return self._entries[error_id]

    def get(self, error_id: str) -> LegacyEntry | None:
        return self._entries.get(error_id)

    def _build(self, path: Path) -> None:
        with open(path, "r", encoding="utf-8", newline="") as f:
            reader = csv.reader(f)
            header = next(reader, None)
            if header != _REQUIRED_HEADER:
                raise ValueError(f"{path}: unexpected header {header!r}, expected {_REQUIRED_HEADER!r}")
            for row in reader:
                if not row:
                    continue
                if len(row) != len(_REQUIRED_HEADER):
                    raise ValueError(f"{path}: expected {len(_REQUIRED_HEADER)} columns, got {len(row)}: {row!r}")
                error_id, runs, overall, test_files, tool_commit, last_date = row
                if error_id in self._entries:
                    raise ValueError(f"{path}: duplicate error_id {error_id!r}")
                self._entries[error_id] = LegacyEntry(
                    runs=int(runs),
                    overall=int(overall),
                    test_files=int(test_files),
                    last_tool_commit=tool_commit,
                    last_date=datetime.fromisoformat(last_date).date(),
                )
