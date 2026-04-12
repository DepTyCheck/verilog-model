from enum import Enum


class MatchingMode(Enum):
    SPECIFIC = 0
    WHOLE = 1


class KnownError:
    """A known error pattern identified by a unique ID and a regex."""

    def __init__(self, error_id: str, pattern: str, mode: MatchingMode) -> None:
        self.error_id = error_id
        self.pattern = pattern
        self.mode = mode


class IgnoredError:
    """An ad-hoc error pattern to ignore (no ID, regex only)."""

    def __init__(self, pattern: str) -> None:
        self.pattern = pattern


class FoundMatch:
    """A regex match found in tool output against a KnownError or IgnoredError."""

    def __init__(self, error: "KnownError | IgnoredError", matched_text: str) -> None:
        self.error = error
        self.matched_text = matched_text


class ErrorMatchInTest:
    """Associates a FoundMatch with the test file path where it occurred."""

    def __init__(self, match: FoundMatch, test_path: str) -> None:
        self.test_path = test_path
        self.match = match


class UnexpectedError:
    """An error from tool output that did not match any known pattern."""

    def __init__(self, tool_output_error_text: str, test_file_path: str) -> None:
        self.tool_output_error_text = tool_output_error_text
        self.test_file_path = test_file_path
