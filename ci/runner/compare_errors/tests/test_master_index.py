import unittest
from datetime import date

from common.first_found_index import FirstFoundIndex
from compare_errors.master_index import MasterIndex
from dataset_stats.files_index import FilesIndex
from dataset_stats.issues_index import IssuesIndex

from ._helpers import FILES, FOUND, ISSUES


def _build(master_commit_date: date, today: date) -> MasterIndex:
    return MasterIndex(
        issues=IssuesIndex(ISSUES),
        files=FilesIndex(FILES),
        first_found=FirstFoundIndex(FOUND),
        master_commit_date=master_commit_date,
        today=today,
    )


class TestMasterIndex(unittest.TestCase):
    def test_error_ids_are_issues_ids(self):
        idx = _build(date(2025, 1, 1), date(2026, 12, 31))
        self.assertEqual(idx.error_ids(), {"alpha", "beta", "delta"})

    def test_alpha_full_window(self):
        # start = max(2025-01-01, first_found 2025-07-19) = 2025-07-19
        # occ = 4 (all alpha rows in window); runs = 4 (all files >= 2025-07-19)
        idx = _build(date(2025, 1, 1), date(2026, 12, 31))
        self.assertAlmostEqual(idx.master_pct("alpha"), 100.0)

    def test_beta_window(self):
        # start = max(2025-01-01, first_found 2025-03-04) = 2025-03-04
        # occ = 1 (2026-01-01); runs = 4 (all files >= 2025-03-04)
        idx = _build(date(2025, 1, 1), date(2026, 12, 31))
        self.assertAlmostEqual(idx.master_pct("beta"), 25.0)

    def test_master_date_excludes_old_occurrences(self):
        # master 2025-09-01 is after both alpha occurrence dates -> occ 0
        # runs in [2025-09-01, today] = 2 (2026-01-01, 2026-02-15)
        idx = _build(date(2025, 9, 1), date(2026, 12, 31))
        self.assertAlmostEqual(idx.master_pct("alpha"), 0.0)

    def test_inclusive_endpoints(self):
        # delta: start = first_found 2026-02-15, today = 2026-02-15
        # occ = 1 (date == start == today); runs = 1 -> 100.0
        idx = _build(date(2025, 1, 1), date(2026, 2, 15))
        self.assertAlmostEqual(idx.master_pct("delta"), 100.0)

    def test_runs_zero_returns_zero(self):
        # master date in the future -> empty window
        idx = _build(date(2027, 1, 1), date(2027, 6, 1))
        self.assertAlmostEqual(idx.master_pct("delta"), 0.0)

    def test_legacy_only_and_yaml_only_return_zero(self):
        # zeta: legacy-only (absent from issues.csv).
        # gamma: has a found_issues yaml but no issues.csv rows.
        idx = _build(date(2025, 1, 1), date(2026, 12, 31))
        self.assertAlmostEqual(idx.master_pct("zeta"), 0.0)
        self.assertAlmostEqual(idx.master_pct("gamma"), 0.0)


if __name__ == "__main__":
    unittest.main()
