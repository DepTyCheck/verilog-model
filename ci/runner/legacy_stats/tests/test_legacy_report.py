import csv
import os
import tempfile
import unittest

from combined_report.previous_report import PreviousReport
from legacy_stats.first_found_index import FirstFoundIndex
from legacy_stats.legacy_report import LegacyReport

DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")
FOUND_OK = os.path.join(DATA_DIR, "found_errors")
PREV_JSON = os.path.join(DATA_DIR, "previous_report.json")
PREV_ZERO_PATHS = os.path.join(DATA_DIR, "previous_report_zero_paths.json")
PREV_ZERO_RUNS = os.path.join(DATA_DIR, "previous_report_zero_runs.json")


class TestLegacyReportBuild(unittest.TestCase):
    def setUp(self):
        prev = PreviousReport(PREV_JSON)
        idx = FirstFoundIndex(FOUND_OK)
        self.report = LegacyReport.build(prev, idx)
        self.by_id = {row.error_id: row for row in self.report.rows}

    def test_alpha_window(self):
        # firstFound=2025-07-19, latest_run=2026-02-15 -> 4 runs in window (totalRuns=40).
        row = self.by_id["alpha"]
        self.assertAlmostEqual(row.occurrence_pct, 75.00)
        self.assertAlmostEqual(row.files_pct, 37.50)
        self.assertAlmostEqual(row.avg_errors_per_file, 2.00)
        self.assertEqual(row.last_commit, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        self.assertEqual(row.last_date, "01.08.2025")

    def test_beta_window(self):
        # firstFound=2025-03-04, latest_run=2026-02-15 -> all 5 runs in window (totalRuns=50).
        row = self.by_id["beta"]
        self.assertAlmostEqual(row.occurrence_pct, 120.00)
        self.assertAlmostEqual(row.files_pct, 60.00)

    def test_gamma_window(self):
        # firstFound=2026-01-01, latest_run=2026-02-15 -> 2 runs in window (totalRuns=20).
        row = self.by_id["gamma"]
        self.assertAlmostEqual(row.occurrence_pct, 25.00)
        self.assertAlmostEqual(row.files_pct, 25.00)
        self.assertAlmostEqual(row.avg_errors_per_file, 1.00)

    def test_delta_first_found_equals_latest_run(self):
        # firstFound=2026-02-15, latest_run=2026-02-15 -> 1 run in window (totalRuns=10).
        row = self.by_id["delta"]
        self.assertAlmostEqual(row.occurrence_pct, 100.00)
        self.assertAlmostEqual(row.files_pct, 100.00)

    def test_sort_descending_occurrence_then_alphabetical(self):
        ids = [row.error_id for row in self.report.rows]
        self.assertEqual(ids, ["beta", "delta", "alpha", "gamma"])


class TestLegacyReportErrors(unittest.TestCase):
    def test_zero_runs_window_raises(self):
        prev = PreviousReport(PREV_ZERO_RUNS)
        idx = FirstFoundIndex(FOUND_OK)
        with self.assertRaises(ValueError) as ctx:
            LegacyReport.build(prev, idx)
        self.assertIn("zero runs in window", str(ctx.exception))

    def test_zero_test_paths_raises(self):
        prev = PreviousReport(PREV_ZERO_PATHS)
        idx = FirstFoundIndex(FOUND_OK)
        with self.assertRaises(ValueError) as ctx:
            LegacyReport.build(prev, idx)
        self.assertIn("test_paths_count == 0", str(ctx.exception))

    def test_unmatched_error_id_raises(self):
        # JSON has 'alpha' but FirstFoundIndex points at a directory containing only 'other_only'
        prev = PreviousReport(PREV_ZERO_RUNS)
        idx = FirstFoundIndex(os.path.join(DATA_DIR, "found_errors_duplicate_only_ids"))
        with self.assertRaises(KeyError):
            LegacyReport.build(prev, idx)


class TestLegacyReportCsv(unittest.TestCase):
    def setUp(self):
        prev = PreviousReport(PREV_JSON)
        idx = FirstFoundIndex(FOUND_OK)
        self.report = LegacyReport.build(prev, idx)

    def test_save_csv_header_and_row_format(self):
        with tempfile.NamedTemporaryFile("r+", suffix=".csv", delete=False, encoding="utf-8") as tmp:
            self.report.save_csv(tmp.name)
            tmp.seek(0)
            reader = list(csv.reader(tmp))
        self.assertEqual(
            reader[0],
            ["error_id", "occurrence_pct", "files_pct", "avg_errors_per_file", "last_commit", "last_date"],
        )
        # First row corresponds to beta (highest occurrence_pct at 120.00)
        self.assertEqual(reader[1][0], "beta")
        self.assertEqual(reader[1][1], "120.00")
        self.assertEqual(reader[1][2], "60.00")
        self.assertEqual(reader[1][3], "2.00")
        self.assertEqual(reader[1][4], "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
        self.assertEqual(reader[1][5], "01.01.2026")
