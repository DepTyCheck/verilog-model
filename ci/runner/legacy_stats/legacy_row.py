from dataclasses import dataclass


@dataclass(frozen=True)
class LegacyRow:
    error_id: str
    runs_for_that_issue: int
    overall_found_count: int
    test_files_count: int
    last_occurrence_tool_commit: str
    last_occurrence_date: str

    @staticmethod
    def csv_header() -> list[str]:
        return [
            "error_id",
            "runs_for_that_issue",
            "overall_found_count",
            "test_files_count",
            "last_occurrence_tool_commit",
            "last_occurrence_date",
        ]

    def to_csv_fields(self) -> list[str]:
        return [
            self.error_id,
            str(self.runs_for_that_issue),
            str(self.overall_found_count),
            str(self.test_files_count),
            self.last_occurrence_tool_commit,
            self.last_occurrence_date,
        ]
