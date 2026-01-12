import unittest

from src.ignored_errors_list import IgnoredErrorsList


class TestIgnoredErrorsList(unittest.TestCase):

    def create_ignored_errors_list(self):
        return IgnoredErrorsList(dir_path="./tests/data/ignored_errors", tool="someTool")

    def test_parsing(self):
        ignored_errors_list = self.create_ignored_errors_list()

        self.assertEqual(len(ignored_errors_list.errors()), 2)
