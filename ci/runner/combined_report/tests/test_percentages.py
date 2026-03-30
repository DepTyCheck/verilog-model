import unittest

from combined_report.percentages import occurrence_pct, total_test_count
from combined_report.report_structure import RunInfo


class TestOccurrencePct(unittest.TestCase):
    def test_zero_total_returns_zero(self):
        self.assertEqual(occurrence_pct(5, 0), 0.0)

    def test_zero_count(self):
        self.assertAlmostEqual(occurrence_pct(0, 100), 0.0)

    def test_full_occurrence(self):
        self.assertAlmostEqual(occurrence_pct(10, 10), 100.0)

    def test_half_occurrence(self):
        self.assertAlmostEqual(occurrence_pct(5, 10), 50.0)

    def test_fractional(self):
        self.assertAlmostEqual(occurrence_pct(1, 4), 25.0)

    def test_over_one_hundred(self):
        # overall can exceed tests_number if multiple errors per test
        self.assertAlmostEqual(occurrence_pct(300, 100), 300.0)


class TestTotalTestCount(unittest.TestCase):
    def test_empty_list(self):
        self.assertEqual(total_test_count([]), 0)

    def test_single_run(self):
        runs = [RunInfo(date="2025-01-01", amount=256)]
        self.assertEqual(total_test_count(runs), 256)

    def test_multiple_runs(self):
        runs = [
            RunInfo(date="2025-01-01", amount=256),
            RunInfo(date="2025-01-02", amount=10),
        ]
        self.assertEqual(total_test_count(runs), 266)
