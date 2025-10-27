from typing import List
from pathlib import Path

from src.tools_report import ToolsReport


class ToolsReportsList:
    def __init__(self, dir_path: str, pattern: str):
        self.reports: List[ToolsReport] = []

        for report_file in Path(dir_path).glob(pattern):
            with open(report_file, "r") as file:
                raw_json = file.read()
                self.reports.append(ToolsReport(raw_json))
