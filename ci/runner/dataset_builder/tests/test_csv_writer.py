import csv
import os
import tempfile
import unittest
from pathlib import Path

from dataset_builder.src.csv_writer import HEADER, append_rows


def _row(**kwargs) -> dict:
    base = {
        "filename": "2026_04_24-seed_111_222.sv",
        "when_issue_occurred": "2026-04-24",
        "tool_name": "slang",
        "error_id": "err_foo",
        "tool_version": "slang 7.0",
        "tool_commit": "abc123",
        "model_commit": "def456",
        "matched_error": "error: foo",
    }
    base.update(kwargs)
    return base


class TestAppendRows(unittest.TestCase):
    def setUp(self):
        fd, self.tmp = tempfile.mkstemp(suffix=".csv")
        os.close(fd)

    def tearDown(self):
        if os.path.exists(self.tmp):
            os.unlink(self.tmp)

    def _read_csv(self) -> list[list[str]]:
        with open(self.tmp, newline="", encoding="utf-8") as f:
            return list(csv.reader(f))

    def test_fails_if_file_missing(self):
        os.unlink(self.tmp)
        with self.assertRaises(FileNotFoundError):
            append_rows(Path(self.tmp), [_row()])

    def test_appends_rows(self):
        append_rows(Path(self.tmp), [_row()])
        rows = self._read_csv()
        self.assertEqual(len(rows), 1)

    def test_appends_multiple_calls(self):
        append_rows(Path(self.tmp), [_row()])
        append_rows(Path(self.tmp), [_row(matched_error="error: bar")])
        rows = self._read_csv()
        self.assertEqual(len(rows), 2)

    def test_matched_error_with_comma_quoted(self):
        append_rows(Path(self.tmp), [_row(matched_error="error: foo, bar")])
        raw = Path(self.tmp).read_text(encoding="utf-8")
        self.assertIn('"error: foo, bar"', raw)

    def test_matched_error_with_semicolon(self):
        append_rows(Path(self.tmp), [_row(matched_error="error: a; b")])
        rows = self._read_csv()
        self.assertEqual(rows[0][HEADER.index("matched_error")], "error: a; b")

    def test_empty_rows_no_error(self):
        append_rows(Path(self.tmp), [])
        rows = self._read_csv()
        self.assertEqual(len(rows), 0)

    def test_column_order_matches_header(self):
        row = _row(filename="x.sv", tool_name="iverilog", error_id="err_bar")
        append_rows(Path(self.tmp), [row])
        rows = self._read_csv()
        data_row = rows[0]
        self.assertEqual(data_row[HEADER.index("filename")], "x.sv")
        self.assertEqual(data_row[HEADER.index("tool_name")], "iverilog")
        self.assertEqual(data_row[HEADER.index("error_id")], "err_bar")


if __name__ == "__main__":
    unittest.main()
