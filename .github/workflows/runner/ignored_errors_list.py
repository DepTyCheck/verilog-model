from pathlib import Path
from typing import List
import re
import sys
import yaml

# Holds a list of regex patterns for errors that should be ignored
class IgnoredErrorsList:
    def __init__(self, dir_path: str, regex_list=None):
        """
        Initialize the IgnoredErrorsList with a directory path containing YAML files and/or a list of extra regexes.
        Args:
            dir_path (str): Path to the directory containing ignored error YAML files
            regex_list (list[str], optional): Additional regexes to ignore
        """
        self._errors: List[dict] = []
        self._extra_regexes: list[str] = []
        self._load_errors(dir_path)
        if regex_list:
            for regex in regex_list:
                regex = regex.rstrip('\n')
                self._extra_regexes.append(regex)

    def _load_errors(self, dir_path: str) -> None:
        """
        Load error patterns from all YAML files in the specified directory.
        Args:
            dir_path (str): Path to the directory containing ignored error YAML files
        """
        dir_path_obj = Path(dir_path)
        if not dir_path_obj.exists():
            print(f"Warning: Directory '{dir_path}' does not exist. No ignored errors loaded.")
            self._errors = []
            return

        yaml_files = list(dir_path_obj.glob("*.yaml"))
        if not yaml_files:
            print(f"Warning: No YAML files found in '{dir_path}'. No ignored errors loaded.")

        errors = []
        for yaml_file in yaml_files:
            try:
                with open(yaml_file, 'r', encoding='utf-8') as f:
                    data = yaml.safe_load(f)
                    error_id = data.get('id')
                    regex = data.get('regex')
                    if error_id is not None and regex is not None:
                        regex = regex.rstrip('\n')
                        errors.append({'id': error_id, 'regex': regex})
                    else:
                        print(f"Warning: {yaml_file} missing 'id' or 'regex', skipping.")
            except Exception as e:
                print(f"Warning: Failed to parse {yaml_file}: {e}")
        self._errors = errors

    def match(self, input_text: str) -> bool:
        """
        Check if any of the error regex patterns match the given input text.
        Args:
            input_text (str): The text to check against the error patterns
        Returns:
            bool: True if any pattern matches, False otherwise
        """
        for error in self._errors:
            pattern = error['regex']
            if re.search(pattern, input_text, re.MULTILINE):
                print(f"Found ignored error. ID: {error['id']} Pattern: {pattern}\n")
                return True
        for pattern in self._extra_regexes:
            if re.search(pattern, input_text, re.MULTILINE):
                print(f"Found ignored error (extra regex): Pattern: {pattern}\n")
                return True
        return False

    @property
    def errors(self) -> List[dict]:
        """
        Get the list of ignored error objects (each with 'id' and 'regex').
        Returns:
            List[dict]: List of error objects to ignore
        """
        return self._errors.copy()

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
