import csv
from dataclasses import dataclass
from datetime import date, datetime
from pathlib import Path


@dataclass(frozen=True)
class IssuesEntry:
    overall_count: int
    distinct_filenames: int
    last_date: date
    last_tool_commit: str
    last_model_commit: str


class IssuesIndex:
    """Aggregates issues.csv rows by error_id.

    issues.csv schema (no header): when_issue_occurred, tool_commit, error_id, model_commit, filename.
    """

    def __init__(self, csv_path: str | Path):
        self._entries: dict[str, IssuesEntry] = {}
        self._build(Path(csv_path))

    def error_ids(self) -> list[str]:
        return list(self._entries)

    def lookup(self, error_id: str) -> IssuesEntry:
        if error_id not in self._entries:
            raise KeyError(f"error_id {error_id!r} not in issues.csv")
        return self._entries[error_id]

    def has(self, error_id: str) -> bool:
        return error_id in self._entries

    def _build(self, path: Path) -> None:
        # Per-id accumulators
        counts: dict[str, int] = {}
        filenames: dict[str, set[str]] = {}
        last_date: dict[str, date] = {}
        last_tool: dict[str, str] = {}
        last_model: dict[str, str] = {}

        with open(path, "r", encoding="utf-8", newline="") as f:
            reader = csv.reader(f)
            for row in reader:
                if not row:
                    continue
                if len(row) != 5:
                    raise ValueError(
                        f"{path}: expected 5 columns, got {len(row)}: {row!r}"
                    )
                when, tool_commit, error_id, model_commit, filename = row
                d = datetime.fromisoformat(when).date()

                counts[error_id] = counts.get(error_id, 0) + 1
                filenames.setdefault(error_id, set()).add(filename)

                # Last-row-wins for any row whose date >= current max
                prev = last_date.get(error_id)
                if prev is None or d >= prev:
                    last_date[error_id] = d
                    last_tool[error_id] = tool_commit
                    last_model[error_id] = model_commit

        for error_id, n in counts.items():
            self._entries[error_id] = IssuesEntry(
                overall_count=n,
                distinct_filenames=len(filenames[error_id]),
                last_date=last_date[error_id],
                last_tool_commit=last_tool[error_id],
                last_model_commit=last_model[error_id],
            )
