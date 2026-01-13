import re
from enum import Enum
from pathlib import Path
from typing import List

import yaml


class MatchingMode(Enum):
    SPECIFIC = 0
    WHOLE = 1


class KnownError:
    """
    Represents a known error pattern that can be ignored, identified by a unique ID and a regex pattern.

    Attributes:
        error_id (str): Unique identifier for the known error.
        pattern (str): Regex pattern that matches the error message.
    """

    def __init__(self, error_id: str, pattern: str, mode: MatchingMode) -> None:
        self.error_id = error_id
        self.pattern = pattern
        self.mode = mode


class IgnoredError:
    """
    Represents an ad-hoc ignored error, specified only by a regex pattern.

    Attributes:
        pattern (str): Regex pattern that matches the error message to ignore.
    """

    def __init__(self, pattern: str) -> None:
        self.pattern = pattern


class FoundMatch:
    """
    Represents a match found in the output for either a KnownError or IgnoredError.

    Attributes:
        error (KnownError | IgnoredError): The error pattern that was matched.
        matched_text (str): The actual text from the output that matched the pattern.
    """

    def __init__(self, error: KnownError | IgnoredError, matched_text: str) -> None:
        self.error = error
        self.matched_text = matched_text


class IgnoredErrorsList:
    def __init__(self, dir_path: str, tool: str, regex_list=None):
        """
        Initialize the IgnoredErrorsList with a directory path containing YAML files and/or a list of extra regexes.
        Args:
            dir_path (str): Path to the directory containing ignored error YAML files
            regex_list (list[str], optional): Additional regexes to ignore
        """
        self._errors: List[KnownError] = []
        self._tool = tool
        self._load_errors(dir_path)

        self._extra_regexes: list[IgnoredError] = []
        if regex_list:
            for regex in regex_list:
                regex = regex.rstrip("\n")
                self._extra_regexes.append(IgnoredError(pattern=regex))

    def _load_errors(self, dir_path: str) -> None:
        """
        Load error patterns from all YAML files in the specified directory.
        Args:
            dir_path (str): Path to the directory containing ignored error YAML files
        """
        dir_path_obj = Path(dir_path)
        if not dir_path_obj.exists():
            print(f"Warning: Directory '{dir_path_obj.absolute()}' does not exist. No ignored errors loaded.")
            self._errors = []
            return

        yaml_files = list(dir_path_obj.glob("*.yaml"))
        if not yaml_files:
            print(f"Warning: No YAML files found in '{dir_path_obj.absolute()}'. No ignored errors loaded.")

        errors: List[KnownError] = []
        for yaml_file in yaml_files:
            try:
                with open(yaml_file, "r", encoding="utf-8") as f:
                    data = yaml.safe_load(f)
                    tool = data.get("tool")
                    if tool != self._tool:
                        continue
                    error_id = data.get("id")
                    pattern = data.get("regex")
                    mode_raw = data.get("matching_mode")
                    mode = MatchingMode.WHOLE if mode_raw == "whole" else MatchingMode.SPECIFIC
                    if error_id is not None and pattern is not None:
                        pattern = pattern.rstrip("\n")
                        errors.append(KnownError(error_id=error_id, pattern=pattern, mode=mode))
                    else:
                        print(f"Warning: {yaml_file} missing 'id' or 'regex', skipping.")
            except Exception as e:
                print(f"Warning: Failed to parse {yaml_file}: {e}")
        self._errors = errors

    def match(self, input_text: str, mode: MatchingMode) -> FoundMatch | None:
        """
        Check if any of the error regex patterns match the given input text.
        Args:
            input_text (str): The text to check against the error patterns
        Returns:
            bool: True if any pattern matches, False otherwise
        """
        known_errors_to_match = [error for error in self._errors if error.mode == mode]

        for error in known_errors_to_match:
            match = re.search(error.pattern, input_text, re.MULTILINE)
            if match:
                print(f"Found ignored error.\nID: {error.error_id}\nPattern: {error.pattern}\n")
                return FoundMatch(error=error, matched_text=match.group(0))

        if mode == MatchingMode.SPECIFIC:
            for ignored_error in self._extra_regexes:
                match = re.search(ignored_error.pattern, input_text, re.MULTILINE)
                if match:
                    print(f"Found ignored error (ignored error): Pattern: {ignored_error.pattern}\n")
                    return FoundMatch(error=ignored_error, matched_text=match.group(0))
        return None

    def __len__(self) -> int:
        """
        Get the number of ignored error patterns.
        Returns:
            int: Number of error patterns
        """
        return len(self._errors)

    def __iter__(self):
        """
        Allow iteration over the ignored error objects.
        Returns:
            Iterator[dict]: Iterator over error objects
        """
        return iter(self._errors)

    def errors(self) -> List[KnownError]:
        return self._errors.copy()
