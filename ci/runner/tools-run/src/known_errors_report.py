import json
from datetime import datetime
from typing import List, Set

from src.error_match_in_test import ErrorMatchInTest
from src.ignored_errors_list import KnownError


class ErrorsCount:
    def __init__(self, error_id: str):
        self.overall = 0  # total number of times this error was found
        self.tests_paths: Set[str] = set()  # paths of tests where this error was found
        self.error_id = error_id

    def to_dict(self):
        test_paths = list(self.tests_paths)
        return {
            "error_id": self.error_id,
            "overall": self.overall,
            "test_paths_count": len(test_paths),
            "tests_paths": test_paths,
        }


class KnownErrorsReport:
    def __init__(self, commit: str):
        self.found_errors: List[ErrorsCount] = []
        self.commit = commit
        self.date = datetime.now().isoformat()

    def add_error(self, error: ErrorMatchInTest):
        if not isinstance(error.match.error, KnownError):
            return
        error_id = error.match.error.error_id

        existing_error = None
        for errors_count in self.found_errors:
            if errors_count.error_id == error_id:
                existing_error = errors_count
                break

        target_error = existing_error if existing_error else ErrorsCount(error_id)
        target_error.tests_paths.add(error.test_path)
        target_error.overall += 1
        if not existing_error:
            self.found_errors.append(target_error)

    def add_errors(self, errors: List[ErrorMatchInTest]):
        for error in errors:
            self.add_error(error)

    def save(self, file_path: str):
        data = {
            "errors": [error.to_dict() for error in self.found_errors],
            "commit": self.commit,
            "date": self.date,
        }
        with open(file_path, "w") as file:
            json.dump(data, file, indent=2)
