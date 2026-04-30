import os
import unittest

from compare_errors.historical_index import HistoricalIndex
from dataset_stats.combined_report import CombinedReport
from dataset_stats.files_index import FilesIndex
from dataset_stats.first_found_index import FirstFoundIndex
from dataset_stats.issues_index import IssuesIndex
from dataset_stats.legacy_index import LegacyIndex

DATASET_DATA_DIR = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    "..",
    "..",
    "dataset_stats",
    "tests",
    "data",
)
ISSUES = os.path.join(DATASET_DATA_DIR, "issues.csv")
FILES = os.path.join(DATASET_DATA_DIR, "files")
FOUND = os.path.join(DATASET_DATA_DIR, "found_issues")
LEGACY = os.path.join(DATASET_DATA_DIR, "legacy_stats.csv")


def _build():
    # Reuses ci/runner/dataset_stats/tests/data/ on purpose — see plan
    # docs/superpowers/plans/2026-04-30-compare-errors-revival.md Task 2.
    report = CombinedReport.build(
        issues=IssuesIndex(ISSUES),
        files=FilesIndex(FILES),
        first_found=FirstFoundIndex(FOUND),
        legacy=LegacyIndex(LEGACY),
    )
    return HistoricalIndex(report)


class TestHistoricalIndex(unittest.TestCase):
    def test_error_ids_contains_legacy_only(self):
        # zeta exists only in legacy_stats.csv per the dataset_stats fixture.
        idx = _build()
        self.assertIn("zeta", idx.error_ids())

    def test_error_ids_contains_issues_only(self):
        # delta exists only in issues.csv per the dataset_stats fixture.
        idx = _build()
        self.assertIn("delta", idx.error_ids())

    def test_pct_alpha_matches_combined_report(self):
        # alpha row: runs=102, overall=54 -> 54/102 * 100
        idx = _build()
        self.assertAlmostEqual(idx.historical_pct("alpha"), 54.0 / 102.0 * 100.0)

    def test_pct_zeta_legacy_only(self):
        # zeta row: runs=50, overall=25 -> 50.0
        idx = _build()
        self.assertAlmostEqual(idx.historical_pct("zeta"), 50.0)

    def test_unknown_id_raises(self):
        idx = _build()
        with self.assertRaises(KeyError):
            idx.historical_pct("does_not_exist")
