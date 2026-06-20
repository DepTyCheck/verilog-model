"""Tests for common/error_file_parser.py — profile (language) is required."""

import tempfile
import unittest
from pathlib import Path

from common.error_file_parser import parse_error_files


def _write(dir_path: Path, name: str, body: str) -> None:
    (dir_path / name).write_text(body, encoding="utf-8")


class TestProfileRequired(unittest.TestCase):

    def test_profile_used_as_language(self):
        with tempfile.TemporaryDirectory() as d:
            _write(Path(d), "e.yaml", "id: E1\nregex: 'error: .*'\nprofile: vhdl\n")
            files = parse_error_files(d)
            self.assertEqual(len(files), 1)
            self.assertEqual(files[0].profile, "vhdl")

    def test_missing_profile_is_skipped(self):
        with tempfile.TemporaryDirectory() as d:
            _write(Path(d), "e.yaml", "id: E1\nregex: 'error: .*'\n")
            self.assertEqual(parse_error_files(d), [])


if __name__ == "__main__":
    unittest.main()
