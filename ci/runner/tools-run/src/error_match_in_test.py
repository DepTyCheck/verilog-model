from src.ignored_errors_list import FoundMatch


class ErrorMatchInTest:
    def __init__(self, match: FoundMatch, test_path: str) -> None:
        self.test_path = test_path
        self.match = match
