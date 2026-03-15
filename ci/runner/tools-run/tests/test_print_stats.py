import unittest
from unittest.mock import patch

from src.print_stats import print_failed_tests_paths
from src.tests_list import TestsRunResult
from src.unexpected_error import UnexpectedError


class TestPrintFailedTestsPaths(unittest.TestCase):

    def test_each_path_printed_once_when_multiple_errors_per_file(self):
        errors = [
            UnexpectedError("error 1", "/path/to/file_a.sv"),
            UnexpectedError("error 2", "/path/to/file_a.sv"),
            UnexpectedError("error 3", "/path/to/file_b.sv"),
        ]
        result = TestsRunResult(run_stats={}, matches=[], unexpected_errors=errors)

        with patch("builtins.print") as mock_print:
            print_failed_tests_paths(result)

        printed = mock_print.call_args[0][0]
        self.assertEqual(printed.count("/path/to/file_a.sv"), 1)
        self.assertEqual(printed.count("/path/to/file_b.sv"), 1)
        self.assertIn("Total failed tests: 2", printed)

    def test_single_error_per_file(self):
        errors = [
            UnexpectedError("error 1", "/path/to/file_a.sv"),
            UnexpectedError("error 2", "/path/to/file_b.sv"),
        ]
        result = TestsRunResult(run_stats={}, matches=[], unexpected_errors=errors)

        with patch("builtins.print") as mock_print:
            print_failed_tests_paths(result)

        printed = mock_print.call_args[0][0]
        self.assertEqual(printed.count("/path/to/file_a.sv"), 1)
        self.assertEqual(printed.count("/path/to/file_b.sv"), 1)
        self.assertIn("Total failed tests: 2", printed)

    def test_no_errors(self):
        result = TestsRunResult(run_stats={}, matches=[], unexpected_errors=[])

        with patch("builtins.print") as mock_print:
            print_failed_tests_paths(result)

        printed = mock_print.call_args[0][0]
        self.assertIn("Total failed tests: 0", printed)
