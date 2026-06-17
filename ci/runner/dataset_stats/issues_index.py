import csv
from dataclasses import dataclass, field
from datetime import date, datetime
from pathlib import Path


@dataclass(frozen=True)
class IssuesEntry:
    overall_count: int
    distinct_filenames: int
    last_date: date
    last_tool_commit: str
    last_model_commit: str


@dataclass
class _Acc:
    count: int = 0
    filenames: set[str] = field(default_factory=set)
    last_date: date | None = None
    last_tool: str = ""
    last_model: str = ""
    dates: list[date] = field(default_factory=list)

    def add(self, when: date, tool_commit: str, model_commit: str, filename: str) -> None:
        self.count += 1
        self.filenames.add(filename)
        self.dates.append(when)
        if self.last_date is None or when >= self.last_date:
            self.last_date = when
            self.last_tool = tool_commit
            self.last_model = model_commit

    def to_entry(self) -> IssuesEntry:
        assert self.last_date is not None
        return IssuesEntry(
            overall_count=self.count,
            distinct_filenames=len(self.filenames),
            last_date=self.last_date,
            last_tool_commit=self.last_tool,
            last_model_commit=self.last_model,
        )


class IssuesIndex:
    """Aggregates issues.csv rows by error_id.

    issues.csv schema (no header): when_issue_occurred, tool_commit, error_id, model_commit, filename.
    """

    def __init__(self, csv_path: str | Path):
        self._entries: dict[str, IssuesEntry] = {}
        self._dates: dict[str, list[date]] = {}
        self._build(Path(csv_path))

    def error_ids(self) -> list[str]:
        return list(self._entries)

    def lookup(self, error_id: str) -> IssuesEntry:
        if error_id not in self._entries:
            raise KeyError(f"error_id {error_id!r} not in issues.csv")
        return self._entries[error_id]

    def has(self, error_id: str) -> bool:
        return error_id in self._entries

    def occurrence_dates(self, error_id: str) -> list[date]:
        return list(self._dates.get(error_id, []))

    def _build(self, path: Path) -> None:
        accs: dict[str, _Acc] = {}
        with open(path, "r", encoding="utf-8", newline="") as f:
            for row in csv.reader(f):
                if not row:
                    continue
                if len(row) != 5:
                    raise ValueError(f"{path}: expected 5 columns, got {len(row)}: {row!r}")
                when, tool_commit, error_id, model_commit, filename = row
                accs.setdefault(error_id, _Acc()).add(
                    datetime.fromisoformat(when).date(),
                    tool_commit,
                    model_commit,
                    filename,
                )
        self._entries = {eid: acc.to_entry() for eid, acc in accs.items()}
        self._dates = {eid: list(acc.dates) for eid, acc in accs.items()}
