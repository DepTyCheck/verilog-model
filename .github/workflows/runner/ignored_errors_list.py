from pathlib import Path
from typing import List
import re
import sys

# Holds a list of regex patterns for errors that should be ignored
class IgnoredErrorsList:
    def __init__(self, file_path: str):
        """
        Initialize the IgnoredErrorsList with a file path.
        
        Args:
            file_path (str): Path to the file containing ignored error patterns
        """
        self._errors: List[str] = []
        self._load_errors(file_path)

    def _load_errors(self, file_path: str) -> None:
        """
        Load error patterns from the specified file.
        
        Args:
            file_path (str): Path to the file containing ignored error patterns
        """
        try:
            with open(file_path, 'r', encoding='utf-8') as file:
                # Read all lines and strip whitespace
                self._errors = [line.strip() for line in file.readlines() if line.strip()]
        except FileNotFoundError:
            print(f"Error: Ignored errors file not found at {file_path}")
            self._errors = []
            sys.exit(1)
        except Exception as e:
            print(f"Error reading ignored errors file: {e}")
            self._errors = []

    def match(self, input_text: str) -> bool:
        """
        Check if any of the error patterns match the given input text.
        
        Args:
            input_text (str): The text to check against the error patterns
            
        Returns:
            bool: True if any pattern matches, False otherwise
        """
        for pattern in self._errors:
            if re.search(pattern, input_text, re.MULTILINE):
                print(f"Found ignored error. Pattern: {pattern}\n")
                return True
        return False

    @property
    def errors(self) -> List[str]:
        """
        Get the list of ignored error patterns.
        
        Returns:
            List[str]: List of error patterns to ignore
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
        Allow iteration over the ignored error patterns.
        
        Returns:
            Iterator[str]: Iterator over error patterns
        """
        return iter(self._errors)
