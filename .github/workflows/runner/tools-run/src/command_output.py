from dataclasses import dataclass

from src.ignored_errors_list import IgnoredErrorsList, FoundMatch
from src.tool_error_regex import ToolErrorRegex
from src.error_match_in_test import ErrorMatchInTest
from src.handle_errors import UnexpectedErrorText


@dataclass
class AnalyzisResult:
    found_matches: list[ErrorMatchInTest]
    unexpected_errors: list[UnexpectedErrorText]


class CommandOutput:
    def __init__(self, stdout: str, stderr: str):
        self.stdout = stdout
        self.stderr = stderr

    def analyze(
        self, ignored_errors_list: IgnoredErrorsList, tool_error_regex: ToolErrorRegex
    ) -> AnalyzisResult:
        pass
