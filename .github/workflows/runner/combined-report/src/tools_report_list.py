from typing import List
from pathlib import Path
import json

from src.tools_report import ToolsReport


class ToolsReportsList:
    def __init__(self, dir_path: str, pattern: str):
        self.reports: List[ToolsReport] = []

        for report_file in Path(dir_path).glob(pattern):
            with open(report_file, "r") as f:
                data_dict = json.load(f)
                self.reports.append(ToolsReport(data_dict))
