import json
from pathlib import Path
from typing import List

from src.tools_report import ToolsReport


class ToolsReportsList:
    def __init__(self, dir_path: str, pattern: str):
        self.reports: List[ToolsReport] = []

        for report_file in Path(dir_path).rglob(pattern):
            if not report_file.is_file():
                continue
            with open(report_file, "r", encoding="utf-8") as f:
                data_dict = json.load(f)
                self.reports.append(ToolsReport(data_dict))
