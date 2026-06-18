import unittest

from common.first_found_index import FirstFoundIndex
from compare_errors.historical_index import HistoricalIndex
from dataset_stats.combined_report import CombinedReport
from dataset_stats.files_index import FilesIndex
from dataset_stats.issues_index import IssuesIndex
from dataset_stats.legacy_index import LegacyIndex

from ._helpers import FILES, FOUND, ISSUES, LEGACY


def _build():
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
