import json

from typing import Dict, List
from src.report_structure import ErrorInfo, RunInfo, LastOccurrence


class ResultReport:
    def __init__(self, errors: Dict[str, ErrorInfo], runs: List[RunInfo]):
        self.errors: Dict[str, ErrorInfo] = errors
        self.runs: List[RunInfo] = runs

    def append_overall(self, error_id: str, count: int, last: LastOccurrence):
        self.errors[error_id].overall += count
        self.errors[error_id].last = last

    def append_test_paths(self, error_id: str, count: int, last: LastOccurrence):
        self.errors[error_id].test_paths_count += count
        self.errors[error_id].last = last

    def append_run(self, date: str, amount: int):
        self.runs.append(RunInfo(date=date, amount=amount))

    def save(self, file_path: str):
        data = {
            "errors": {error_id: error.to_dict() for error_id, error in self.errors.items()},
            "runs": [run.to_dict() for run in self.runs],
        }
        with open(file_path, "w") as file:
            json.dump(data, file, indent=2)
