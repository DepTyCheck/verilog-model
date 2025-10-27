import json
from collections import Counter

from src.tools_report_list import ToolsReportsList
from src.previous_report import PreviousReport
from src.result_report import ResultReport


class CombinedReport:
    def __init__(self, previous_report: PreviousReport, tools_reports_list: ToolsReportsList, tests_number: int):
        self.previous_report = previous_report
        self.overall_errors_counter: Counter = Counter()
        self.test_paths_errors_counter: Counter = Counter()
        self.last_issue_occurrence = {}
        self.tests_number = tests_number

        for report in tools_reports_list.reports:
            for error in report.errors:
                self.overall_errors_counter[error.error_id] += error.overall
                self.test_paths_errors_counter[error.error_id] += error.test_paths_count
                self.last_issue_occurrence[error.error_id] = {
                    "commit": error.commit,
                    "date": error.date,
                }

    def combine(self) -> ResultReport:
        result = ResultReport(self.previous_report.errors, self.previous_report.runs)
        for error_id, count in self.overall_errors_counter.items():
            result.append_overall(error_id, count, self.last_issue_occurrence[error_id])
        for error_id, count in self.test_paths_errors_counter.items():
            result.append_test_paths(error_id, count, self.last_issue_occurrence[error_id])
        return result
