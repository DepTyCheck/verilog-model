"""Tests for common/unknown_error_reporter.py"""

import io
import json
import os
import tempfile
import unittest

from common.command_output import AnalyzisResult
from common.error_types import UnexpectedError
from common.tool_matrix_runner import FileInput


def _fi(name: str = "f.sv") -> FileInput:
    return FileInput(content="", file_suffix=".sv", context=None, logical_name=name)


def _clean() -> AnalyzisResult:
    return AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)


def _fail(path: str, text: str) -> AnalyzisResult:
    return AnalyzisResult(
        found_matches=[],
        unexpected_errors=[UnexpectedError(tool_output_error_text=text, test_file_path=path)],
        all_errors_are_known=False,
    )


class TestCollectUnknownErrors(unittest.TestCase):

    def test_empty_results_returns_empty(self):
        from common.unknown_error_reporter import collect_unknown_errors

        self.assertEqual(collect_unknown_errors([]), [])

    def test_clean_results_return_empty(self):
        from common.unknown_error_reporter import collect_unknown_errors

        entries = collect_unknown_errors([(_fi(), _clean()), (_fi(), _clean())])
        self.assertEqual(entries, [])

    def test_failed_result_produces_entry(self):
        from common.unknown_error_reporter import collect_unknown_errors

        results = [(_fi("a.sv"), _fail("a.sv", "error msg"))]
        entries = collect_unknown_errors(results)
        self.assertEqual(len(entries), 1)
        self.assertEqual(entries[0].file_path, "a.sv")
        self.assertEqual(entries[0].error_text, "error msg")

    def test_mixed_results_only_failed_collected(self):
        from common.unknown_error_reporter import collect_unknown_errors

        results = [
            (_fi("a.sv"), _clean()),
            (_fi("b.sv"), _fail("b.sv", "boom")),
            (_fi("c.sv"), _clean()),
        ]
        entries = collect_unknown_errors(results)
        self.assertEqual(len(entries), 1)
        self.assertEqual(entries[0].file_path, "b.sv")

    def test_multiple_unexpected_per_result_all_collected(self):
        from common.unknown_error_reporter import collect_unknown_errors

        result = AnalyzisResult(
            found_matches=[],
            unexpected_errors=[
                UnexpectedError("err1", "f.sv"),
                UnexpectedError("err2", "f.sv"),
            ],
            all_errors_are_known=False,
        )
        entries = collect_unknown_errors([(_fi("f.sv"), result)])
        self.assertEqual(len(entries), 2)


class TestSaveUnknownErrorsJson(unittest.TestCase):

    def test_roundtrip(self):
        from common.unknown_error_reporter import UnknownErrorEntry, collect_unknown_errors, save_unknown_errors_json

        results = [(_fi("a.sv"), _fail("a.sv", "kaboom"))]
        entries = collect_unknown_errors(results)

        with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as f:
            path = f.name
        try:
            save_unknown_errors_json(entries, path)
            with open(path) as fh:
                data = json.load(fh)
        finally:
            os.unlink(path)

        self.assertIsInstance(data, list)
        self.assertEqual(len(data), 1)
        self.assertEqual(data[0]["file_path"], "a.sv")
        self.assertEqual(data[0]["error_text"], "kaboom")

    def test_empty_entries_writes_empty_array(self):
        from common.unknown_error_reporter import save_unknown_errors_json

        with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as f:
            path = f.name
        try:
            save_unknown_errors_json([], path)
            with open(path) as fh:
                data = json.load(fh)
        finally:
            os.unlink(path)
        self.assertEqual(data, [])


class TestPrintUnknownErrors(unittest.TestCase):

    def test_prints_file_path_and_text(self):
        from contextlib import redirect_stdout

        from common.unknown_error_reporter import UnknownErrorEntry, print_unknown_errors

        entries = [UnknownErrorEntry(file_path="a.sv", error_text="bad syntax")]
        captured = io.StringIO()
        with redirect_stdout(captured):
            print_unknown_errors(entries)
        output = captured.getvalue()
        self.assertIn("a.sv", output)
        self.assertIn("bad syntax", output)

    def test_empty_entries_no_output(self):
        from contextlib import redirect_stdout

        from common.unknown_error_reporter import print_unknown_errors

        captured = io.StringIO()
        with redirect_stdout(captured):
            print_unknown_errors([])
        self.assertEqual(captured.getvalue(), "")


if __name__ == "__main__":
    unittest.main()
