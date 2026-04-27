from dataclasses import dataclass


@dataclass(frozen=True)
class LegacyRow:
    error_id: str
    occurrence_pct: float
    files_pct: float
    avg_errors_per_file: float
    last_commit: str
    last_date: str

    @staticmethod
    def csv_header() -> list[str]:
        return ["error_id", "occurrence_pct", "files_pct", "avg_errors_per_file", "last_commit", "last_date"]

    def to_csv_fields(self) -> list[str]:
        return [
            self.error_id,
            f"{self.occurrence_pct:.2f}",
            f"{self.files_pct:.2f}",
            f"{self.avg_errors_per_file:.2f}",
            self.last_commit,
            self.last_date,
        ]
