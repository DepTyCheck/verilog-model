from dataclasses import dataclass


@dataclass
class ErrorReport:
    error_id: str
    overall: int
    test_paths_count: int
    tests_paths: list[str]


class ToolsReport:
    def __init__(self, data: dict):
        self.errors: list[ErrorReport] = self.parse_errors(data)
        self.commit = data["commit"]
        self.date = data["date"]

    def parse_errors(self, data: dict) -> list[ErrorReport]:
        return [ErrorReport(**error) for error in data.get("errors", [])]
