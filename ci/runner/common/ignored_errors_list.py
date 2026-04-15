import re
from typing import List

from common.error_file_parser import ErrorFile, parse_error_files
from common.error_types import FoundMatch, IgnoredError, KnownError, MatchingMode
from common.logger import get_logger


class IgnoredErrorsList:
    def __init__(self, dir_path: str, tool: str, regex_list=None):
        """
        Initialize with a directory of YAML error files and optional extra regexes.

        Args:
            dir_path: Path to the directory containing ignored error YAML files.
            tool: Tool name to filter YAML files by.
            regex_list: Additional regex strings to ignore (beyond YAML files).
        """
        self._tool = tool
        self._errors: List[KnownError] = self._load_errors(dir_path, tool)

        self._extra_regexes: list[IgnoredError] = []
        if regex_list:
            for regex in regex_list:
                regex = regex.rstrip("\n")
                self._extra_regexes.append(IgnoredError(pattern=regex))

    @classmethod
    def from_patterns(cls, patterns: List[str], mode: "MatchingMode" = None) -> "IgnoredErrorsList":
        """Build an IgnoredErrorsList directly from regex pattern strings (for tests)."""
        from common.error_types import MatchingMode as _Mode
        mode = mode if mode is not None else _Mode.SPECIFIC
        instance = cls.__new__(cls)
        instance._tool = None
        instance._errors = [KnownError(error_id=f"e{i}", pattern=p, mode=mode) for i, p in enumerate(patterns)]
        instance._extra_regexes = []
        return instance

    @classmethod
    def from_error_files(cls, error_files: List[ErrorFile], extra_regexes=None) -> "IgnoredErrorsList":
        """Build an IgnoredErrorsList directly from a pre-loaded list of ErrorFile objects."""
        instance = cls.__new__(cls)
        instance._tool = None
        instance._errors = [KnownError(error_id=ef.error_id, pattern=ef.regex, mode=ef.mode) for ef in error_files]
        instance._extra_regexes = [IgnoredError(pattern=r.rstrip("\n")) for r in (extra_regexes or [])]
        return instance

    @staticmethod
    def _load_errors(dir_path: str, tool: str) -> List[KnownError]:
        error_files = parse_error_files(dir_path, tool=tool)
        return [KnownError(error_id=ef.error_id, pattern=ef.regex, mode=ef.mode) for ef in error_files]

    def match(self, input_text: str, mode: MatchingMode) -> FoundMatch | None:
        """Check if any known error pattern matches input_text in the given mode."""
        for error in self._errors:
            if error.mode != mode:
                continue
            m = re.search(error.pattern, input_text, re.MULTILINE)
            if m:
                get_logger().info(f"Found ignored error.\nID: {error.error_id}\nPattern: {error.pattern}\n")
                return FoundMatch(error=error, matched_text=m.group(0))

        if mode == MatchingMode.SPECIFIC:
            for ignored_error in self._extra_regexes:
                m = re.search(ignored_error.pattern, input_text, re.MULTILINE)
                if m:
                    get_logger().info(f"Found ignored error (extra): Pattern: {ignored_error.pattern}\n")
                    return FoundMatch(error=ignored_error, matched_text=m.group(0))
        return None

    def __len__(self) -> int:
        return len(self._errors)

    def __iter__(self):
        return iter(self._errors)

    def errors(self) -> List[KnownError]:
        return self._errors.copy()
