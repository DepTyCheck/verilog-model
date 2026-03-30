from combined_report import PreviousReport, ToolsReportsList, total_test_count, occurrence_pct


class ErrorPercentageDelta:
    def __init__(self, error_id: str, historical_pct: float, current_pct: float):
        self.error_id = error_id
        self.historical_pct = historical_pct
        self.current_pct = current_pct
        self.delta_pct = current_pct - historical_pct


class ErrorsComparison:
    def __init__(
        self,
        previous_report: PreviousReport,
        tools_reports_list: ToolsReportsList,
        tests_number: int,
    ):
        self.previous_report = previous_report
        self.tools_reports_list = tools_reports_list
        self.tests_number = tests_number

    def current_run_counts(self) -> dict[str, int]:
        counts: dict[str, int] = {}
        for tool_report in self.tools_reports_list.reports:
            for error_report in tool_report.errors:
                counts[error_report.error_id] = counts.get(error_report.error_id, 0) + error_report.overall
        return counts

    def compare(self) -> list[ErrorPercentageDelta]:
        current_counts = self.current_run_counts()
        all_ids = set(self.previous_report.errors.keys()) | set(current_counts.keys())

        historical_total = total_test_count(self.previous_report.runs)

        deltas = []
        for error_id in all_ids:
            prev_overall = self.previous_report.errors[error_id].overall if error_id in self.previous_report.errors else 0
            historical_pct = occurrence_pct(prev_overall, historical_total)
            current_pct = occurrence_pct(current_counts.get(error_id, 0), self.tests_number)
            deltas.append(ErrorPercentageDelta(
                error_id=error_id,
                historical_pct=historical_pct,
                current_pct=current_pct,
            ))

        deltas.sort(key=lambda d: abs(d.delta_pct), reverse=True)
        return deltas
