import os
import unittest
from datetime import date

from dataset_stats.legacy_index import LegacyIndex

DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")
LEGACY = os.path.join(DATA_DIR, "legacy_stats.csv")


class TestLegacyIndex(unittest.TestCase):
    def setUp(self):
        self.idx = LegacyIndex(LEGACY)

    def test_error_ids(self):
        self.assertEqual(set(self.idx.error_ids()), {"alpha", "beta", "zeta"})

    def test_alpha(self):
        e = self.idx.lookup("alpha")
        self.assertEqual(e.runs, 100)
        self.assertEqual(e.overall, 50)
        self.assertEqual(e.test_files, 40)
        self.assertEqual(e.last_tool_commit, "legacyA")
        self.assertEqual(e.last_date, date(2025, 6, 15))

    def test_lookup_missing_raises(self):
        with self.assertRaises(KeyError):
            self.idx.lookup("nope")

    def test_get_returns_none(self):
        self.assertIsNone(self.idx.get("nope"))


if __name__ == "__main__":
    unittest.main()
