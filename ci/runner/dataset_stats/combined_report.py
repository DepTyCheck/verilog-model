import csv
from datetime import date
from pathlib import Path

from dataset_stats.combined_row import CombinedRow
from dataset_stats.files_index import FilesIndex
from dataset_stats.first_found_index import FirstFoundIndex
from dataset_stats.issues_index import IssuesIndex
from dataset_stats.legacy_index import LegacyIndex


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

        if issues.has(error_id):
            ff = first_found.lookup(error_id)  # KeyError if missing
            entry = issues.lookup(error_id)
            new_overall = entry.overall_count
            new_test_files = entry.distinct_filenames
            new_last_date: date | None = entry.last_date
            new_runs = files.count_in_window(ff, entry.last_date)
            new_tool = entry.last_tool_commit
            new_model = entry.last_model_commit
        else:
            new_overall = 0
            new_test_files = 0
            new_runs = 0
            new_last_date = None
            new_tool = ""
            new_model = ""

        runs = (leg.runs if leg else 0) + new_runs
        overall = (leg.overall if leg else 0) + new_overall
        test_files = (leg.test_files if leg else 0) + new_test_files

        if runs == 0:
            raise ValueError(f"{error_id}: zero runs combined across legacy and issues.csv")

        if new_last_date is not None and (leg is None or new_last_date >= leg.last_date):
            last_date = new_last_date
            last_tool = new_tool
            last_model = new_model
        else:
            assert leg is not None
            last_date = leg.last_date
            last_tool = leg.last_tool_commit
            last_model = ""

        return CombinedRow(
            error_id=error_id,
            runs_for_that_issue=runs,
            overall_found_count=overall,
            test_files_count=test_files,
            last_occurrence_tool_commit=last_tool,
            last_occurrence_date=last_date.isoformat(),
            last_model_commit=last_model,
        )

    def save_csv(self, path: str | Path) -> None:
        path_obj = Path(path)
        if str(path_obj) == "-":
            import sys

            self._write(sys.stdout)
        else:
            with open(path_obj, "w", encoding="utf-8", newline="") as f:
                self._write(f)

    def _write(self, fh) -> None:
        writer = csv.writer(fh)
        writer.writerow(CombinedRow.csv_header())
        for row in self.rows:
            writer.writerow(row.to_csv_fields())
