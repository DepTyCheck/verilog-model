"""Shared schema for the legacy/combined stats CSV rows.

The dataset and the legacy CSV reports share a six-column prefix; the dataset
report appends one extra column. Both schemas live here so call sites import
the canonical header and a single typed row class instead of repeating the
column list and dataclass shape.
"""

from dataclasses import dataclass

CSV_HEADER: list[str] = [
    "error_id",
    "runs_for_that_issue",
    "overall_found_count",
    "test_files_count",
    "last_occurrence_tool_commit",
    "last_occurrence_date",
]


@dataclass(frozen=True)
class StatsRow:
    error_id: str
    runs_for_that_issue: int
    overall_found_count: int
    test_files_count: int
    last_occurrence_tool_commit: str
    last_occurrence_date: str

    @classmethod
    def csv_header(cls) -> list[str]:
        return list(CSV_HEADER)

    def to_csv_fields(self) -> list[str]:
        return [
            self.error_id,
            str(self.runs_for_that_issue),
            str(self.overall_found_count),
            str(self.test_files_count),
            self.last_occurrence_tool_commit,
            self.last_occurrence_date,
        ]
