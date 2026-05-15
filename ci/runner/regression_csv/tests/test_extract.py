import tempfile
import unittest
from pathlib import Path

from regression_csv.extract import (
    extract_rows_from_report,
    tool_name_from_artifact,
)
from regression_csv.tests.helpers import clean_file, known_error_file, write_artifact


class TestToolNameFromArtifact(unittest.TestCase):
    def test_basic(self):
        self.assertEqual(
            tool_name_from_artifact("regression-iverilog-per-file.json"),
            "iverilog",
        )

    def test_tool_with_hyphens(self):
        self.assertEqual(
            tool_name_from_artifact("regression-tree-sitter-systemverilog-per-file.json"),
            "tree-sitter-systemverilog",
        )

    def test_unrecognised_returns_none(self):
        self.assertIsNone(tool_name_from_artifact("garbage.json"))


class TestExtractRowsFromReport(unittest.TestCase):
    def test_reproduced_true_when_match_id_equals_expected(self):
        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "regression-iverilog-per-file.json"
            write_artifact(
                p,
                "iverilog",
                [known_error_file("foo-minified.sv", "err_a", "boom")],
            )
            expected = {("foo", "minified"): "err_a"}
            rows = list(extract_rows_from_report(p, expected))
            self.assertEqual(rows, [("foo", "minified", True)])

    def test_reproduced_false_when_no_match(self):
        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "regression-iverilog-per-file.json"
            write_artifact(p, "iverilog", [clean_file("foo-minified.sv")])
            expected = {("foo", "minified"): "err_a"}
            rows = list(extract_rows_from_report(p, expected))
            self.assertEqual(rows, [("foo", "minified", False)])

    def test_unmapped_filename_skipped(self):
        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "regression-iverilog-per-file.json"
            write_artifact(p, "iverilog", [clean_file("ghost-minified.sv")])
            expected = {("foo", "minified"): "err_a"}
            rows = list(extract_rows_from_report(p, expected))
            self.assertEqual(rows, [])


if __name__ == "__main__":
    unittest.main()
