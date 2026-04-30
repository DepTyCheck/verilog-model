import os
import unittest
from datetime import date

from dataset_stats.files_index import FilesIndex

DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")
FILES_OK = os.path.join(DATA_DIR, "files")
FILES_BAD = os.path.join(DATA_DIR, "files_bad_name")


class TestFilesIndex(unittest.TestCase):
    def setUp(self):
        self.idx = FilesIndex(FILES_OK)

    def test_full_window(self):
        self.assertEqual(
            self.idx.count_in_window(date(2025, 1, 1), date(2026, 12, 31)), 4
        )

    def test_partial_window(self):
        # Includes 2025-07-19 and 2025-08-01
        self.assertEqual(
            self.idx.count_in_window(date(2025, 7, 19), date(2025, 8, 1)), 2
        )

    def test_inclusive_endpoints(self):
        # Both endpoints inclusive
        self.assertEqual(
            self.idx.count_in_window(date(2025, 7, 19), date(2025, 7, 19)), 1
        )

    def test_empty_window(self):
        # Window before any file
        self.assertEqual(
            self.idx.count_in_window(date(2024, 1, 1), date(2024, 12, 31)), 0
        )

    def test_window_endpoints_reversed_returns_zero(self):
        self.assertEqual(
            self.idx.count_in_window(date(2026, 12, 31), date(2025, 1, 1)), 0
        )

    def test_includes_vhdl_files(self):
        # 2026-02-15 is a .vhdl file
        self.assertEqual(
            self.idx.count_in_window(date(2026, 2, 15), date(2026, 2, 15)), 1
        )


class TestFilesIndexBadName(unittest.TestCase):
    def test_unparseable_filename_raises(self):
        with self.assertRaises(ValueError) as ctx:
            FilesIndex(FILES_BAD)
        self.assertIn("not_a_dated_file.sv", str(ctx.exception))


if __name__ == "__main__":
    unittest.main()
