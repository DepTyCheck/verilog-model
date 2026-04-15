"""Tests for common/tool_matrix_runner.py"""

import unittest
from unittest.mock import MagicMock, patch

from common.command_config import CommandConfig
from common.command_output import AnalyzisResult
from common.error_types import UnexpectedError
from common.tool_matrix_runner import FileInput, ResultCollector, run_all


def _clean() -> AnalyzisResult:
    return AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)


def _fail(path: str = "f.sv") -> AnalyzisResult:
    return AnalyzisResult(
        found_matches=[],
        unexpected_errors=[UnexpectedError("boom", path)],
        all_errors_are_known=False,
    )


class TestFileInput(unittest.TestCase):

    def test_fields_stored(self):
        fi = FileInput(content="abc", file_suffix=".sv", context="ctx")
        self.assertEqual(fi.content, "abc")
        self.assertEqual(fi.file_suffix, ".sv")
        self.assertEqual(fi.context, "ctx")
        self.assertIsNone(fi.assets)
        self.assertIsNone(fi.logical_name)

    def test_optional_assets_and_name(self):
        assets = MagicMock()
        fi = FileInput(content="x", file_suffix=".sv", context=None, assets=assets, logical_name="/orig/path.sv")
        self.assertIs(fi.assets, assets)
        self.assertEqual(fi.logical_name, "/orig/path.sv")


class TestResultCollector(unittest.TestCase):

    def test_starts_empty(self):
        rc = ResultCollector()
        self.assertEqual(rc.results(), [])
        self.assertFalse(rc.has_unknown_errors())

    def test_accumulates_pairs(self):
        rc = ResultCollector()
        fi = FileInput(content="a", file_suffix=".sv", context=None)
        result = _clean()
        rc.handle(fi, result)
        self.assertEqual(len(rc.results()), 1)
        self.assertIs(rc.results()[0][0], fi)
        self.assertIs(rc.results()[0][1], result)

    def test_has_unknown_errors_false_when_all_clean(self):
        rc = ResultCollector()
        fi = FileInput(content="a", file_suffix=".sv", context=None)
        rc.handle(fi, _clean())
        self.assertFalse(rc.has_unknown_errors())

    def test_has_unknown_errors_true_when_any_fail(self):
        rc = ResultCollector()
        rc.handle(FileInput(content="a", file_suffix=".sv", context=None), _clean())
        rc.handle(FileInput(content="b", file_suffix=".sv", context=None), _fail())
        self.assertTrue(rc.has_unknown_errors())


class TestRunAll(unittest.TestCase):

    @patch("common.tool_matrix_runner.run_file")
    def test_run_all_calls_run_file_for_each_input(self, mock_run_file):
        mock_run_file.return_value = _clean()

        inputs = [
            FileInput(content="a", file_suffix=".sv", context=1),
            FileInput(content="b", file_suffix=".sv", context=2),
        ]
        commands = [CommandConfig(run="tool {file}")]
        known_errors = MagicMock()
        collector = ResultCollector()

        run_all(inputs, commands, known_errors, collector)

        self.assertEqual(mock_run_file.call_count, 2)
        self.assertEqual(len(collector.results()), 2)

    @patch("common.tool_matrix_runner.run_file")
    def test_run_all_passes_logical_name(self, mock_run_file):
        mock_run_file.return_value = _clean()

        fi = FileInput(content="x", file_suffix=".sv", context=None, logical_name="/orig.sv")
        collector = ResultCollector()
        run_all([fi], [CommandConfig(run="t {file}")], MagicMock(), collector)

        _, kwargs = mock_run_file.call_args
        self.assertEqual(kwargs["logical_name"], "/orig.sv")

    @patch("common.tool_matrix_runner.run_file")
    def test_run_all_stateless_no_return(self, mock_run_file):
        mock_run_file.return_value = _clean()
        collector = ResultCollector()
        result = run_all([], [], MagicMock(), collector)
        self.assertIsNone(result)


if __name__ == "__main__":
    unittest.main()
