import os
import unittest
from datetime import date

from dataset_stats.issues_index import IssuesIndex

DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")
ISSUES = os.path.join(DATA_DIR, "issues.csv")


class TestIssuesIndex(unittest.TestCase):
    def setUp(self):
        self.idx = IssuesIndex(ISSUES)

    def test_known_ids(self):
        self.assertEqual(set(self.idx.error_ids()), {"alpha", "beta", "delta"})

    def test_overall_count_alpha(self):
        # 4 rows for alpha
        self.assertEqual(self.idx.lookup("alpha").overall_count, 4)

    def test_distinct_filenames_alpha(self):
        # 2 distinct filenames for alpha (one is duplicated across 3 rows)
        self.assertEqual(self.idx.lookup("alpha").distinct_filenames, 2)

    def test_last_date_alpha(self):
        self.assertEqual(self.idx.lookup("alpha").last_date, date(2025, 8, 1))

    def test_last_row_wins_alpha(self):
        # Last row at 2025-08-01 has tool_commit=tcB, model_commit=mc2
        e = self.idx.lookup("alpha")
        self.assertEqual(e.last_tool_commit, "tcB")
        self.assertEqual(e.last_model_commit, "mc2")

    def test_single_row_beta(self):
        e = self.idx.lookup("beta")
        self.assertEqual(e.overall_count, 1)
        self.assertEqual(e.distinct_filenames, 1)
        self.assertEqual(e.last_date, date(2026, 1, 1))
        self.assertEqual(e.last_tool_commit, "tcA")
        self.assertEqual(e.last_model_commit, "mc3")

    def test_lookup_missing_raises(self):
        with self.assertRaises(KeyError):
            self.idx.lookup("nonexistent")


if __name__ == "__main__":
    unittest.main()
