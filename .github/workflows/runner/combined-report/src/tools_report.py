from dataclasses import dataclass
from typing import List
import json


@dataclass
class ErrorReport:
    error_id: str
    overall: int
    test_paths_count: int
    commit: str
    date: str


class ToolsReport:
    def __init__(self, raw_json: str):
        self.errors: List[ErrorReport] = self.parse(raw_json)

    def parse(self, raw_json: str) -> List[ErrorReport]:
        data = json.loads(raw_json)
        return [ErrorReport(**error) for error in data.get("errors", [])]
