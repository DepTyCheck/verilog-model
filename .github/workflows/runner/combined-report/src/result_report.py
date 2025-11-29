import json

from typing import List
from src.report_structure import ErrorInfo, RunInfo


class ResultReport:
    def __init__(self, errors: dict[str, ErrorInfo], runs: List[RunInfo]):
        self.errors: dict[str, ErrorInfo] = errors
        self.runs: List[RunInfo] = runs

    def save(self, file_path: str):
        data = {
            "errors": {
                error_id: error.to_dict() for error_id, error in self.errors.items()
            },
            "runs": [run.to_dict() for run in self.runs],
        }
        with open(file_path, "w") as file:
            json.dump(data, file, indent=2)
