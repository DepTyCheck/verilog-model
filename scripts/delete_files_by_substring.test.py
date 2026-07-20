#!/usr/bin/env python3

import csv
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
from delete_files_by_substring import clean, file_contains  # noqa: E402


class DeleteFilesBySubstringTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp_dir = tempfile.TemporaryDirectory()
        self.root = Path(self.temp_dir.name)
        self.files_dir = self.root / "files"
        self.files_dir.mkdir()
        self.csv_path = self.root / "issues.csv"

    def tearDown(self) -> None:
        self.temp_dir.cleanup()

    def write_csv(self, rows: list[list[str]]) -> None:
        with self.csv_path.open("w", newline="", encoding="utf-8") as output:
            csv.writer(output).writerows(rows)

    def read_csv(self) -> list[list[str]]:
        with self.csv_path.open(newline="", encoding="utf-8") as source:
            return list(csv.reader(source))

    def test_deletes_matching_files_and_all_referencing_rows(self) -> None:
        matching = "matching.sv"
        retained = "retained.sv"
        (self.files_dir / matching).write_text("module remove_me; endmodule")
        (self.files_dir / retained).write_text("module keep_me; endmodule")
        self.write_csv(
            [
                ["date", "issue-a", matching],
                ["date", "issue-b", retained],
                ["date", "issue-c", matching],
            ]
        )

        result = clean("remove", self.csv_path, self.files_dir)

        self.assertEqual(result, (1, 2))
        self.assertFalse((self.files_dir / matching).exists())
        self.assertTrue((self.files_dir / retained).exists())
        self.assertEqual(self.read_csv(), [["date", "issue-b", retained]])

    def test_content_match_is_a_literal_substring_not_a_regex(self) -> None:
        literal_match = "literal.sv"
        regex_only_match = "regex-only.sv"
        (self.files_dir / literal_match).write_text("wire design[1];")
        (self.files_dir / regex_only_match).write_text("wire design1;")
        self.write_csv([["issue", literal_match], ["issue", regex_only_match]])

        result = clean("[1]", self.csv_path, self.files_dir)

        self.assertEqual(result, (1, 1))
        self.assertFalse((self.files_dir / literal_match).exists())
        self.assertTrue((self.files_dir / regex_only_match).exists())
        self.assertEqual(self.read_csv(), [["issue", regex_only_match]])

    def test_dry_run_changes_nothing(self) -> None:
        filename = "dry-run.sv"
        (self.files_dir / filename).write_text("please delete this")
        rows = [["issue", filename]]
        self.write_csv(rows)

        result = clean("delete", self.csv_path, self.files_dir, dry_run=True)

        self.assertEqual(result, (1, 1))
        self.assertTrue((self.files_dir / filename).exists())
        self.assertEqual(self.read_csv(), rows)

    def test_empty_substring_is_rejected(self) -> None:
        self.write_csv([])
        with self.assertRaises(ValueError):
            clean("", self.csv_path, self.files_dir)

    def test_finds_substring_across_read_chunks(self) -> None:
        path = self.files_dir / "chunked.sv"
        path.write_bytes(b"abcdeNEEDLEtail")
        self.assertTrue(file_contains(path, b"NEEDLE", chunk_size=8))


if __name__ == "__main__":
    unittest.main()
