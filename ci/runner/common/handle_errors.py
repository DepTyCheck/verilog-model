import re
from typing import Protocol

from common.error_types import ErrorMatchInTest, FoundMatch, MatchingMode, UnexpectedError
from common.logger import get_logger
from common.tool_error_regex import ToolErrorRegex


class ErrorMatcherProtocol(Protocol):
    """
    Any object that can match tool output against known error patterns.
    Satisfied by tools_run's IgnoredErrorsList without explicit inheritance.
    """

    def match(self, input_text: str, mode: MatchingMode) -> FoundMatch | None: ...


class ExtractedErrorsByToolRegex:
    """Extract per-error matches from output using the tool error regex."""

    def __init__(
        self,
        output: str,
        tool_regex: ToolErrorRegex,
        ignored_errors: ErrorMatcherProtocol,
        test_path: str,
    ):
        self.unexpected_errors: list[UnexpectedError] = []
        self.found_matches: list[ErrorMatchInTest] = []

        get_logger().info("Matching errors from output")

        matches = list(re.finditer(tool_regex.regex, output, re.MULTILINE))
        if not matches:
            get_logger().warning("Warning: No errors matched.\n")
            print(f"[DEBUG] error_regex: {tool_regex.regex!r}", flush=True)
            print(f"[DEBUG] tool output:\n{output}", flush=True)
        else:
            for match in matches:
                error_text = match.group(0)
                get_logger().info(f"Matched error: {error_text}")
                found_match = ignored_errors.match(error_text, mode=MatchingMode.SPECIFIC)
                if found_match is None:
                    get_logger().info(f"\033[91mFound unexpected error: {error_text}\033[0m\n")
                    self.unexpected_errors.append(
                        UnexpectedError(
                            tool_output_error_text=error_text,
                            test_file_path=test_path,
                        )
                    )
                else:
                    self.found_matches.append(ErrorMatchInTest(match=found_match, test_path=test_path))

    def some_matches_found(self):
        return len(self.found_matches) > 0 or len(self.unexpected_errors) > 0

    def all_errors_are_known(self):
        return len(self.unexpected_errors) == 0


class WholeOutputMatch:
    """Check if the whole output matches any known error pattern (WHOLE mode)."""

    def __init__(self, output: str, ignored_errors: ErrorMatcherProtocol):
        get_logger().info("Matching whole output")
        self.found_match = ignored_errors.match(output, mode=MatchingMode.WHOLE)
        if self.found_match is None:
            get_logger().info("\033[91mCouldn't match whole output\033[0m\n")
