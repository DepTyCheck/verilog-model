import csv
from datetime import datetime
from pathlib import Path

from combined_report.previous_report import PreviousReport
from legacy_stats.first_found_index import FirstFoundIndex
from legacy_stats.legacy_row import LegacyRow


class LegacyReport:
    def __init__(self, rows: list[LegacyRow]):
        self.rows = rows

    @classmethod
    def build(cls, prev: PreviousReport, idx: FirstFoundIndex) -> "LegacyReport":
        rows: list[LegacyRow] = []
        for error_id, info in prev.errors.items():
            first_found = idx.lookup(error_id)
            last_date = datetime.fromisoformat(info.last.date).date()

            total_runs = sum(run.amount for run in prev.runs if first_found <= datetime.fromisoformat(run.date).date() <= last_date)

            if total_runs == 0:
                raise ValueError(f"{error_id}: zero runs in window [{first_found.isoformat()}, {last_date.isoformat()}]")
            if info.test_paths_count == 0:
                raise ValueError(f"{error_id}: test_paths_count == 0")

            rows.append(
                LegacyRow(
                    error_id=error_id,
                    occurrence_pct=round(info.overall / total_runs * 100, 2),
                    files_pct=round(info.test_paths_count / total_runs * 100, 2),
                    avg_errors_per_file=round(info.overall / info.test_paths_count, 2),
                    last_commit=info.last.commit,
                    last_date=last_date.strftime("%d.%m.%Y"),
                )
            )

        rows.sort(key=lambda r: (-r.occurrence_pct, r.error_id))
        return cls(rows)

    def save_csv(self, path: str | Path) -> None:
        with open(path, "w", encoding="utf-8", newline="") as f:
            writer = csv.writer(f)
            writer.writerow(LegacyRow.csv_header())
            for row in self.rows:
                writer.writerow(row.to_csv_fields())
