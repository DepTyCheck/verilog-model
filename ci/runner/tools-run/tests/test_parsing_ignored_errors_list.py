import unittest

from src.ignored_errors_list import IgnoredErrorsList


def create_ignored_errors_list(tool: str):
    return IgnoredErrorsList(dir_path="./tests/data/ignored_errors", tool=tool)


class TestIgnoredErrorsList(unittest.TestCase):

    def test_parsing(self):
        ignored_errors_list = create_ignored_errors_list(tool="someTool")

        self.assertEqual(len(ignored_errors_list.errors()), 2)
