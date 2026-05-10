from dataset_stats.combined_report import CombinedReport


class HistoricalIndex:
    """Per-error_id occurrence percentages over the dataset branch.

    Wraps a `dataset_stats.CombinedReport`. The percentage uses the
    per-error window denominator already computed by CombinedReport
    (`runs_for_that_issue`).
    """

    def __init__(self, combined_report: CombinedReport):
        self._rows = {row.error_id: row for row in combined_report.rows}

    def error_ids(self) -> set[str]:
        return set(self._rows)

    def historical_pct(self, error_id: str) -> float:
        if error_id not in self._rows:
            raise KeyError(f"error_id {error_id!r} not in historical index")
        row = self._rows[error_id]
        return row.overall_found_count / row.runs_for_that_issue * 100.0
