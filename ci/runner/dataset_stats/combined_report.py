import csv
import sys
from dataclasses import dataclass
from datetime import date
from pathlib import Path

from common.first_found_index import FirstFoundIndex
from dataset_stats.combined_row import CombinedRow
from dataset_stats.files_index import FilesIndex
from dataset_stats.issues_index import IssuesIndex
from dataset_stats.legacy_index import LegacyIndex


@dataclass(frozen=True)
class _NewFields:
    overall: int
    test_files: int
    last_date: date | None
    runs: int
    tool: str
    model: str


@dataclass(frozen=True)
class _LastOccurrence:
    date: date
    tool: str
    model: str


class CombinedReport:
    def __init__(self, rows: list[CombinedRow]):
        self.rows = rows

    @classmethod
    def build(
        cls,
        issues: IssuesIndex,
        files: FilesIndex,
        first_found: FirstFoundIndex,
        legacy: LegacyIndex,
    ) -> "CombinedReport":
        ids = set(legacy.error_ids()) | set(issues.error_ids())
        rows: list[CombinedRow] = []
        for error_id in ids:
            rows.append(cls._row_for(error_id, issues, files, first_found, legacy))
        rows.sort(key=lambda r: (-r.overall_found_count, r.error_id))
        return cls(rows)

    @staticmethod
    def _row_for(
        error_id: str,
        issues: IssuesIndex,
        files: FilesIndex,
        first_found: FirstFoundIndex,
        legacy: LegacyIndex,
    ) -> CombinedRow:
        leg = legacy.get(error_id)
        new = CombinedReport._new_fields(error_id, issues, files, first_found)

        runs = (leg.runs if leg else 0) + new.runs
        if runs == 0:
            raise ValueError(f"{error_id}: zero runs combined across legacy and issues.csv")

        last = CombinedReport._pick_last_occurrence(leg, new)

        return CombinedRow(
            error_id=error_id,
            runs_for_that_issue=runs,
            overall_found_count=(leg.overall if leg else 0) + new.overall,
            test_files_count=(leg.test_files if leg else 0) + new.test_files,
            last_occurrence_tool_commit=last.tool,
            last_occurrence_date=last.date.isoformat(),
            last_model_commit=last.model,
        )

    @staticmethod
    def _new_fields(
        error_id: str,
        issues: IssuesIndex,
        files: FilesIndex,
        first_found: FirstFoundIndex,
    ) -> _NewFields:
        if not issues.has(error_id):
            return _NewFields(overall=0, test_files=0, last_date=None, runs=0, tool="", model="")
        ff = first_found.lookup(error_id)  # KeyError if missing
        entry = issues.lookup(error_id)
        return _NewFields(
            overall=entry.overall_count,
            test_files=entry.distinct_filenames,
            last_date=entry.last_date,
            runs=files.count_in_window(ff, entry.last_date),
            tool=entry.last_tool_commit,
            model=entry.last_model_commit,
        )

    @staticmethod
    def _pick_last_occurrence(leg, new: _NewFields) -> _LastOccurrence:
        if new.last_date is not None and (leg is None or new.last_date >= leg.last_date):
            return _LastOccurrence(date=new.last_date, tool=new.tool, model=new.model)
        assert leg is not None
        return _LastOccurrence(date=leg.last_date, tool=leg.last_tool_commit, model="")

    def save_csv(self, path: str | Path) -> None:
        path_obj = Path(path)
        if str(path_obj) == "-":
            self._write(sys.stdout)
        else:
            with open(path_obj, "w", encoding="utf-8", newline="") as f:
                self._write(f)

    def _write(self, fh) -> None:
        writer = csv.writer(fh)
        writer.writerow(CombinedRow.csv_header())
        for row in self.rows:
            writer.writerow(row.to_csv_fields())
