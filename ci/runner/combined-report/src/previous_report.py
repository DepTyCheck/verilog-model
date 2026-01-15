import json
from typing import Dict, List

from src.report_structure import ErrorInfo, LastOccurrence, RunInfo


class PreviousReport:
    def __init__(self, file_path: str):
        self.errors: Dict[str, ErrorInfo] = {}
        self.runs: List[RunInfo] = []

        self.errors, self.runs = self.parse(file_path)

    def parse(self, file_path: str):
        with open(file_path, "r", encoding="utf-8") as file:
            raw_json = file.read()

        data = json.loads(raw_json)

        errors: Dict[str, ErrorInfo] = {}
        runs: List[RunInfo] = []

        errors_data = data.get("errors", {})
        for error_id, error_info in errors_data.items():
            last_occurrence = LastOccurrence(commit=error_info["last"]["commit"], date=error_info["last"]["date"])
            errors[error_id] = ErrorInfo(
                overall=error_info["overall"],
                test_paths_count=error_info["test_paths_count"],
                last=last_occurrence,
            )

        runs_data = data.get("runs", [])
        runs = [RunInfo(date=run["date"], amount=run["amount"]) for run in runs_data]

        return errors, runs
