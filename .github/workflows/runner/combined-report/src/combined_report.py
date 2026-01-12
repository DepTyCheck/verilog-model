from datetime import datetime

from src.previous_report import PreviousReport
from src.report_structure import ErrorInfo, LastOccurrence, RunInfo
from src.result_report import ResultReport
from src.tools_report_list import ToolsReportsList


class CombinedReport:
    def __init__(
        self,
        previous_report: PreviousReport,
        tools_reports_list: ToolsReportsList,
        tests_number: int,
    ):
        self.previous_report = previous_report
        self.tools_reports_list = tools_reports_list
        self.tests_number = tests_number
        self.date = datetime.now().isoformat()

    def combined_runs(self) -> list[RunInfo]:
        return self.previous_report.runs + [RunInfo(date=self.date, amount=self.tests_number)]

    def combined_errors(self) -> dict[str, ErrorInfo]:
        data: dict[str, ErrorInfo] = self.previous_report.errors.copy()

        for tool_report in self.tools_reports_list.reports:
            for error_report in tool_report.errors:
                err_id = error_report.error_id
                last = LastOccurrence(commit=tool_report.commit, date=self.date)

                if err_id in list(data.keys()):
                    data[err_id].overall += error_report.overall
                    data[err_id].test_paths_count += error_report.test_paths_count
                    data[err_id].last = last
                else:
                    data[err_id] = ErrorInfo(
                        overall=error_report.overall,
                        test_paths_count=error_report.test_paths_count,
                        last=last,
                    )

        return data

    def combine(self) -> ResultReport:
        return ResultReport(
            errors=self.combined_errors(),
            runs=self.combined_runs(),
        )
