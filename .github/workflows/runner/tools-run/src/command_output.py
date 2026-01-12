from dataclasses import dataclass

from src.ignored_errors_list import IgnoredErrorsList, FoundMatch
from src.unexpected_error import UnexpectedError
from src.tool_error_regex import ToolErrorRegex
from src.error_match_in_test import ErrorMatchInTest
from src.handle_errors import ExtractedErrorsByToolRegex, WholeOutputMatch


@dataclass
class AnalyzisResult:
    found_matches: list[ErrorMatchInTest]
    unexpected_errors: list[UnexpectedError]
    all_errors_are_known: bool


class FoundWholeMatch(AnalyzisResult):
    def __init__(self, found_match: FoundMatch, test_path: str):
        self.found_matches = [ErrorMatchInTest(match=found_match, test_path=test_path)]
        self.unexpected_errors = []
        self.all_errors_are_known = True


class NotFoundWholeMatch(AnalyzisResult):
    def __init__(self, error_text: str, test_path: str):
        self.found_matches = []
        self.unexpected_errors = [UnexpectedError(tool_output_error_text=error_text, test_file_path=test_path)]
        self.all_errors_are_known = False


class CommandOutput:
    def __init__(self, out: str):
        self.out = out

    def analyze(
        self,
        ignored_errors_list: IgnoredErrorsList,
        tool_error_regex: ToolErrorRegex,
        file_path: str,
    ) -> AnalyzisResult:
        # Match errors
        extacted_errors = ExtractedErrorsByToolRegex(
            self.out,
            tool_error_regex,
            ignored_errors_list,
            file_path,
        )

        if extacted_errors.some_matches_found():
            return AnalyzisResult(
                found_matches=extacted_errors.found_matches,
                unexpected_errors=extacted_errors.unexpected_errors,
                all_errors_are_known=extacted_errors.all_errors_are_known(),
            )
        else:
            # Match whole output
            whole_output_match = WholeOutputMatch(self.out, ignored_errors_list)

            if whole_output_match.found_match != None:
                return FoundWholeMatch(
                    found_match=whole_output_match.found_match,
                    test_path=file_path,
                )
            else:
                return NotFoundWholeMatch(
                    error_text="\n".join(self.out.splitlines()[:3]),
                    test_path=file_path,
                )
