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

    def test_alpha_window_two_runs(self):
        row = self.by_id["alpha"]
        self.assertAlmostEqual(row.occurrence_pct, 150.00)
        self.assertAlmostEqual(row.files_pct, 75.00)
        self.assertAlmostEqual(row.avg_errors_per_file, 2.00)
        self.assertEqual(row.last_commit, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        self.assertEqual(row.last_date, "01.08.2025")

    def test_beta_window_four_runs(self):
        row = self.by_id["beta"]
        self.assertAlmostEqual(row.occurrence_pct, 150.00)
        self.assertAlmostEqual(row.files_pct, 75.00)

    def test_gamma_single_run_window(self):
        row = self.by_id["gamma"]
        self.assertAlmostEqual(row.occurrence_pct, 50.00)
        self.assertAlmostEqual(row.files_pct, 50.00)
        self.assertAlmostEqual(row.avg_errors_per_file, 1.00)

    def test_delta_first_found_equals_last_date(self):
        row = self.by_id["delta"]
        self.assertAlmostEqual(row.occurrence_pct, 100.00)
        self.assertAlmostEqual(row.files_pct, 100.00)

    def test_sort_descending_occurrence_then_alphabetical(self):
        ids = [row.error_id for row in self.report.rows]
        self.assertEqual(ids, ["alpha", "beta", "delta", "gamma"])


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
        # First row corresponds to alpha (tie at 150.00, alpha < beta)
        self.assertEqual(reader[1][0], "alpha")
        self.assertEqual(reader[1][1], "150.00")
        self.assertEqual(reader[1][2], "75.00")
        self.assertEqual(reader[1][3], "2.00")
        self.assertEqual(reader[1][4], "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        self.assertEqual(reader[1][5], "01.08.2025")
