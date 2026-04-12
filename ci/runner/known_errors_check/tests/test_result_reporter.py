"""Unit tests for known_errors_check/src/result_reporter.py."""

import json
import os
import shutil
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from common.command_config import CommandConfig
from common.error_file_parser import parse_error_files
from common.run_command import ExecutionResult
from common.tool_error_regex import ToolErrorRegex
from known_errors_check.src.error_checker import ErrorResult, ExampleResult, ToolConfig, check_all
from known_errors_check.src.result_reporter import build_report, format_markdown_table, print_summary, save_report

DATA_DIR = str(Path(__file__).parent / "data")


def _make_example_result(name: str, reproduced: bool) -> ExampleResult:
    return ExampleResult(example_name=name, example_type="minified", reproduced=reproduced)


class TestBuildReport(unittest.TestCase):

    def test_reproduced_maps_to_true(self):
        er = ErrorResult(error_id="some_error", originating_tool="iverilog")
        er.examples.append(_make_example_result("ex1", reproduced=True))

        report = build_report([er])

        self.assertEqual(report, {"some_error": {"ex1-minified": True}})

    def test_not_reproduced_maps_to_false(self):
        er = ErrorResult(error_id="some_error", originating_tool="iverilog")
        er.examples.append(_make_example_result("ex1", reproduced=False))

        report = build_report([er])

        self.assertEqual(report, {"some_error": {"ex1-minified": False}})

    def test_multiple_examples(self):
        er = ErrorResult(error_id="e1", originating_tool="iverilog")
        er.examples.append(_make_example_result("ex1", reproduced=True))
        er.examples.append(_make_example_result("ex2", reproduced=False))

        report = build_report([er])

        self.assertEqual(report, {"e1": {"ex1-minified": True, "ex2-minified": False}})

    def test_multiple_errors(self):
        er1 = ErrorResult(error_id="e1", originating_tool="iverilog")
        er1.examples.append(_make_example_result("ex1", reproduced=True))

        er2 = ErrorResult(error_id="e2", originating_tool="slang")
        er2.examples.append(_make_example_result("ex1", reproduced=False))

        report = build_report([er1, er2])

        self.assertEqual(report, {"e1": {"ex1-minified": True}, "e2": {"ex1-minified": False}})

    def test_empty_returns_empty_dict(self):
        self.assertEqual(build_report([]), {})

    def test_error_with_no_examples(self):
        er = ErrorResult(error_id="e1", originating_tool="iverilog")
        report = build_report([er])
        self.assertEqual(report, {"e1": {}})


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
    End-to-end report test using the real two_examples_with_full.yaml data file.

    Loads the file, mocks tool execution so all four examples reproduce the known
    error, then prints the JSON report, regression summary, and Markdown table to
    stdout so the output is visible when running pytest -s.
    """

    def _fake_result(self, output: str, ok: bool = False) -> ExecutionResult:
        return ExecutionResult(
            command_executed_successfully=True,
            result_code_is_ok=ok,
            timed_out=False,
            output=output,
        )

    @patch("known_errors_check.src.error_checker.run_command")
    @patch("known_errors_check.src.error_checker.make_command")
    def test_report_with_all_four_examples_reproducing(self, mock_make, mock_run):
        mock_make.return_value = "fake_tool file.sv"
        mock_run.return_value = self._fake_result("known error pattern here")

        error_files = parse_error_files(DATA_DIR, tool="fake_tool")
        target = next(f for f in error_files if f.error_id == "two_examples_with_full")
        self.assertEqual(len(target.examples), 4, "expected 4 Example objects from the data file")

        tool = ToolConfig(
            name="fake_tool",
            commands=[CommandConfig(run="fake_tool {file}", error_regex=ToolErrorRegex("known error pattern here"))],
        )

        with tempfile.TemporaryDirectory() as tmp:
            tool_dir = os.path.join(tmp, "fake_tool")
            os.makedirs(tool_dir)
            src = Path(DATA_DIR) / "two_examples_with_full.yaml"

            shutil.copy(src, tool_dir)

            error_results, new_errors, regressions = check_all(tmp, tool)

        self.assertEqual(new_errors, [])
        self.assertEqual(len(regressions), 4)

        report = build_report(error_results)

        # print("\n=== JSON report ===")
        # print(json.dumps(report, indent=2))

        # print("\n=== Markdown table ===")
        # print(format_markdown_table(report))

        # print("\n=== Console summary ===")
        # print_summary(error_results, new_errors, regressions)
