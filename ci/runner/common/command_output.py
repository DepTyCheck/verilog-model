from dataclasses import dataclass

from common.error_types import ErrorMatchInTest, FoundMatch, UnexpectedError
from common.handle_errors import ErrorMatcherProtocol, ExtractedErrorsByToolRegex, WholeOutputMatch
from common.tool_error_regex import ToolErrorRegex


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
        ignored_errors_list: ErrorMatcherProtocol,
        tool_error_regex: ToolErrorRegex,
        file_path: str,
    ) -> AnalyzisResult:
        extracted_errors = ExtractedErrorsByToolRegex(
            self.out,
            tool_error_regex,
            ignored_errors_list,
            file_path,
        )

        if extracted_errors.some_matches_found():
            return AnalyzisResult(
                found_matches=extracted_errors.found_matches,
                unexpected_errors=extracted_errors.unexpected_errors,
                all_errors_are_known=extracted_errors.all_errors_are_known(),
            )
        else:
            whole_output_match = WholeOutputMatch(self.out, ignored_errors_list)
            if whole_output_match.found_match is not None:
                return FoundWholeMatch(found_match=whole_output_match.found_match, test_path=file_path)
            else:
                return NotFoundWholeMatch(
                    error_text="\n".join(self.out.splitlines()[:3]),
                    test_path=file_path,
                )
