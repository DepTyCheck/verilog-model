from dataclasses import dataclass

from common.stats_csv import CSV_HEADER, StatsRow


@dataclass(frozen=True)
class CombinedRow(StatsRow):
    last_model_commit: str = ""

    @classmethod
    def csv_header(cls) -> list[str]:
        return [*CSV_HEADER, "last_model_commit"]

    def to_csv_fields(self) -> list[str]:
        return [*super().to_csv_fields(), self.last_model_commit]
