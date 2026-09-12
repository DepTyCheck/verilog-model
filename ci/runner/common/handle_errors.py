import re
from typing import Protocol

from common.error_types import (
    ErrorMatchInTest,
    FoundMatch,
    IgnoredError,
    KnownError,
    MatchingMode,
    UnexpectedError,
)
from common.line_group_errors import carve_group, group_atoms
from common.logger import get_logger
from common.tool_error_regex import ToolErrorRegex


class ErrorMatcherProtocol(Protocol):
    """
    Any object that can match tool output against known error patterns.
    Satisfied by common.ignored_errors_list.IgnoredErrorsList without explicit inheritance.
    """

    def match(self, input_text: str, mode: MatchingMode) -> FoundMatch | None: ...

    def specific_matchers_in_order(self) -> list[KnownError | IgnoredError]: ...


class ExtractedErrorsByToolRegex:
    """Extract per-error matches from output using the tool error regex."""

    def __init__(
        self,
        output: str,
        tool_regex: ToolErrorRegex,
        ignored_errors: ErrorMatcherProtocol,
        test_path: str,
        location_regex: str | None = None,
    ):
        self.unexpected_errors: list[UnexpectedError] = []
        self.found_matches: list[ErrorMatchInTest] = []

        get_logger().info("Matching errors from output")

        atoms = [m.group(0) for m in re.finditer(tool_regex.regex, output, re.MULTILINE)]

        if not atoms:
            get_logger().warning(f"No errors matched by tool regex: {tool_regex.regex!r}")
            get_logger().debug(f"Tool output:\n{output}")
            return

        if location_regex is not None:
            self._match_with_grouping(atoms, ignored_errors, test_path, location_regex)
        else:
            self._match_legacy(atoms, ignored_errors, test_path)

    def _match_legacy(
        self,
        atoms: list[str],
        ignored_errors: ErrorMatcherProtocol,
        test_path: str,
    ) -> None:
        for error_text in atoms:
            get_logger().debug(f"Extracted error: {error_text!r}")
            found_match = ignored_errors.match(error_text, mode=MatchingMode.SPECIFIC)
            if found_match is None:
                get_logger().warning(f"Unmatched error (not in known errors): {error_text!r}")
                self.unexpected_errors.append(
                    UnexpectedError(
                        tool_output_error_text=error_text,
                        test_file_path=test_path,
                    )
                )
            else:
                error_id = getattr(found_match.error, "error_id", None)
                label = f"[{error_id}]" if error_id else "(extra regex)"
                get_logger().debug(f"Matched known error {label}: {error_text!r}")
                self.found_matches.append(ErrorMatchInTest(match=found_match, test_path=test_path))

    def _match_with_grouping(
        self,
        atoms: list[str],
        ignored_errors: ErrorMatcherProtocol,
        test_path: str,
        location_regex: str,
    ) -> None:
        groups = group_atoms(atoms, location_regex)
        patterns = [(m, m.pattern) for m in ignored_errors.specific_matchers_in_order()]

        for group in groups:
            hits, leftover = carve_group(group.atoms, patterns)
            for error_obj, matched_text in hits:
                found_match = FoundMatch(error=error_obj, matched_text=matched_text)
                error_id = getattr(error_obj, "error_id", None)
                label = f"[{error_id}]" if error_id else "(extra regex)"
                get_logger().debug(f"Grouped match {label}: {matched_text!r}")
                self.found_matches.append(ErrorMatchInTest(match=found_match, test_path=test_path))
            for atom in leftover:
                get_logger().warning(f"Unmatched error in group (not in known errors): {atom!r}")
                self.unexpected_errors.append(
                    UnexpectedError(
                        tool_output_error_text=atom,
                        test_file_path=test_path,
                    )
                )

    def some_matches_found(self):
        return len(self.found_matches) > 0 or len(self.unexpected_errors) > 0

    def all_errors_are_known(self):
        return len(self.unexpected_errors) == 0


class WholeOutputMatch:
    """Check if the whole output matches any known error pattern (WHOLE mode)."""

    def __init__(self, output: str, ignored_errors: ErrorMatcherProtocol):
        get_logger().debug("Trying whole-output match")
        self.found_match = ignored_errors.match(output, mode=MatchingMode.WHOLE)
        if self.found_match is None:
            get_logger().warning("Whole-output match also failed — error is unknown")
