"""Unit tests for regression_test/src/result_reporter.py."""

import json
import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from common.command_config import CommandConfig
from common.command_output import AnalyzisResult
from common.error_file_parser import ErrorFile, Example, parse_error_files
from common.error_types import ErrorMatchInTest, FoundMatch, KnownError, MatchingMode
from common.tool_error_regex import ToolErrorRegex
from common.tool_matrix_runner import FileInput, ResultCollector, run_all
from regression_test.src.error_checker import iter_regression_inputs
from regression_test.src.result_reporter import build_reproducibility_report, format_markdown_table, save_report
from tools_run.src.ignored_errors_list import IgnoredErrorsList

DATA_DIR = str(Path(__file__).parent / "data")


class TestFormatMarkdownTable(unittest.TestCase):

    def test_empty_report_returns_fallback(self):
        result = format_markdown_table({})
        self.assertIn("No known errors", result)

    def test_contains_header_columns(self):
        report = {"some_error": {"ex1": True}}
        table = format_markdown_table(report)
        self.assertIn("Error ID", table)
        self.assertIn("Example", table)
        self.assertIn("Reproduced", table)

    def test_reproduced_true_shows_checkmark(self):
        table = format_markdown_table({"err": {"ex": True}})
        self.assertIn("✅", table)

    def test_reproduced_false_shows_cross(self):
        table = format_markdown_table({"err": {"ex": False}})
        self.assertIn("❌", table)

    def test_error_id_plain_without_prefix(self):
        table = format_markdown_table({"my_error": {"ex": True}})
        self.assertIn("my_error", table)
        self.assertNotIn("](", table)

    def test_error_id_is_link_with_prefix(self):
        table = format_markdown_table(
            {"my_error": {"ex": True}},
            error_url_prefix="https://example.com/error",
        )
        self.assertIn("[my_error](https://example.com/error/my_error)", table)

    def test_multiple_examples_each_on_own_row(self):
        table = format_markdown_table({"err": {"ex1": True, "ex2": False}})
        self.assertIn("ex1", table)
        self.assertIn("ex2", table)
        self.assertIn("✅", table)
        self.assertIn("❌", table)

    def test_multiple_errors_all_present(self):
        table = format_markdown_table({"err1": {"a": True}, "err2": {"b": False}})
        self.assertIn("err1", table)
        self.assertIn("err2", table)

    def test_contains_title(self):
        table = format_markdown_table({"e": {"ex": True}})
        self.assertIn("Known Errors Regression Check", table)


class TestSaveReport(unittest.TestCase):

    def test_report_written_as_valid_json(self):
        report = {"some_error": {"ex1": True, "ex2": False}}
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            path = f.name
        try:
            save_report(report, path)
            with open(path, encoding="utf-8") as f:
                loaded = json.load(f)
            self.assertEqual(loaded, {"some_error": {"ex1": True, "ex2": False}})
        finally:
            os.unlink(path)


class TestReportForTwoExamplesWithFull(unittest.TestCase):
    """
    Regression report test using the real two_examples_with_full.yaml data file.

    Loads the file, mocks run_file so all four examples reproduce the known error,
    then calls run_all() + build_reproducibility_report() and asserts outcomes.
    """

    @patch("common.tool_matrix_runner.run_file")
    def test_report_with_all_four_examples_reproducing(self, mock_run_file):
        all_error_files = parse_error_files(DATA_DIR, tool="fake_tool")
        target = next(f for f in all_error_files if f.error_id == "two_examples_with_full")
        self.assertEqual(len(target.examples), 4, "expected 4 Example objects from the data file")

        error_regex = "known error pattern here"
        known = KnownError(error_id="two_examples_with_full", pattern=error_regex, mode=MatchingMode.SPECIFIC)

        def make_reproduced_result(content, commands, known_errors, file_suffix, assets=None, logical_name=None):
            match = FoundMatch(error=known, matched_text=error_regex)
            return AnalyzisResult(
                found_matches=[ErrorMatchInTest(match=match, test_path=logical_name or "f.sv")],
                unexpected_errors=[],
                all_errors_are_known=True,
            )

        mock_run_file.side_effect = make_reproduced_result

        commands = [CommandConfig(run="fake_tool {file}", error_regex=ToolErrorRegex(error_regex))]
        known_errors = IgnoredErrorsList.from_error_files([target])

        collector = ResultCollector()
        run_all(iter_regression_inputs([target], ".sv"), commands, known_errors, collector)

        results = collector.results()
        self.assertEqual(len(results), 4, "expected 4 (FileInput, AnalyzisResult) pairs")

        report = build_reproducibility_report(results, "fake_tool")

        self.assertIn("two_examples_with_full", report)
        for key, reproduced in report["two_examples_with_full"].items():
            self.assertTrue(reproduced, f"Expected {key} to be reproduced")


# ---------------------------------------------------------------------------
# build_reproducibility_report
# ---------------------------------------------------------------------------


class TestBuildReproducibilityReport(unittest.TestCase):

    def _make_results(self, error_id: str, tool: str, example_name: str, reproduced: bool):
        """Build a (FileInput, AnalyzisResult) pair simulating a run outcome."""
        ef = ErrorFile(
            error_id=error_id,
            tool=tool,
            regex="pat",
            mode=MatchingMode.SPECIFIC,
            title="",
            language="sv",
            examples=[Example(name=example_name, type="minified", content="c")],
        )
        example = ef.examples[0]
        fi = FileInput(content="c", file_suffix=".sv", context=(ef, example))

        if reproduced:
            known = KnownError(error_id=error_id, pattern="pat", mode=MatchingMode.SPECIFIC)
            match = FoundMatch(error=known, matched_text="pat")
            result = AnalyzisResult(
                found_matches=[ErrorMatchInTest(match=match, test_path="f.sv")],
                unexpected_errors=[],
                all_errors_are_known=True,
            )
        else:
            result = AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)

        return fi, result

    def test_reproduced_true(self):
        fi, result = self._make_results("err1", "tool-a", "ex1", reproduced=True)
        report = build_reproducibility_report([(fi, result)], "tool-a")
        self.assertTrue(report["err1"]["ex1-minified"])

    def test_not_reproduced_false(self):
        fi, result = self._make_results("err1", "tool-a", "ex1", reproduced=False)
        report = build_reproducibility_report([(fi, result)], "tool-a")
        self.assertFalse(report["err1"]["ex1-minified"])

    def test_other_tool_errors_excluded_from_report(self):
        fi, result = self._make_results("err_b", "tool-b", "ex1", reproduced=False)
        report = build_reproducibility_report([(fi, result)], "tool-a")
        # tool-b error not owned by tool-a → not in report
        self.assertNotIn("err_b", report)
