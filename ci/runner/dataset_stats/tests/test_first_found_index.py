import os
import unittest
from datetime import date

from dataset_stats.first_found_index import FirstFoundIndex

DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")
FOUND_OK = os.path.join(DATA_DIR, "found_issues")
FOUND_MISSING_FF = os.path.join(DATA_DIR, "found_issues_missing_first_found")
FOUND_UNPARSEABLE = os.path.join(DATA_DIR, "found_issues_unparseable")
FOUND_DUPLICATE = os.path.join(DATA_DIR, "found_issues_duplicate")


class TestFirstFoundIndexHappyPath(unittest.TestCase):
    def setUp(self):
        self.idx = FirstFoundIndex(FOUND_OK)

    def test_single_example(self):
        self.assertEqual(self.idx.lookup("alpha"), date(2025, 7, 19))

    def test_multiple_examples_earliest_wins(self):
        self.assertEqual(self.idx.lookup("beta"), date(2025, 3, 4))

    def test_separate_tool_dirs(self):
        self.assertEqual(self.idx.lookup("delta"), date(2026, 2, 15))

    def test_missing_id_raises(self):
        with self.assertRaises(KeyError):
            self.idx.lookup("not_present")


class TestFirstFoundIndexErrors(unittest.TestCase):
    def test_missing_first_found_raises(self):
        with self.assertRaises(ValueError) as ctx:
            FirstFoundIndex(FOUND_MISSING_FF)
        self.assertIn("no parseable first_found", str(ctx.exception))

    def test_unparseable_first_found_raises(self):
        with self.assertRaises(ValueError) as ctx:
            FirstFoundIndex(FOUND_UNPARSEABLE)
        self.assertIn("unparseable first_found", str(ctx.exception))

    def test_duplicate_id_across_yaml_files_raises(self):
        with self.assertRaises(ValueError) as ctx:
            FirstFoundIndex(FOUND_DUPLICATE)
        self.assertIn("duplicate error_id", str(ctx.exception))


if __name__ == "__main__":
    unittest.main()
