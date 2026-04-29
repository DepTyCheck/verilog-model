import csv
import os
import tempfile
import unittest

from combined_report.previous_report import PreviousReport
from legacy_stats.first_found_index import FirstFoundIndex
from legacy_stats.legacy_report import LegacyReport
from legacy_stats.legacy_row import LegacyRow

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
        # firstFound=2025-07-19, last.date=2025-08-01 -> runs in window: 07-19, 08-01 (amount=10 each) -> 20
        row = self.by_id["alpha"]
        self.assertEqual(row.runs_for_that_issue, 20)
        self.assertEqual(row.overall_found_count, 30)
        self.assertEqual(row.test_files_count, 15)
        self.assertEqual(row.last_occurrence_tool_commit, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        self.assertEqual(row.last_occurrence_date, "2025-08-01")

    def test_beta_window(self):
        # firstFound=2025-03-04, last.date=2026-01-01 -> 4 runs in window -> 40
        row = self.by_id["beta"]
        self.assertEqual(row.runs_for_that_issue, 40)
        self.assertEqual(row.overall_found_count, 60)
        self.assertEqual(row.test_files_count, 30)
        self.assertEqual(row.last_occurrence_date, "2026-01-01")

    def test_gamma_window(self):
        # firstFound=2026-01-01, last.date=2026-01-01 -> 1 run -> 10
        row = self.by_id["gamma"]
        self.assertEqual(row.runs_for_that_issue, 10)
        self.assertEqual(row.overall_found_count, 5)
        self.assertEqual(row.test_files_count, 5)
        self.assertEqual(row.last_occurrence_date, "2026-01-01")

    def test_delta_first_found_equals_last_date(self):
        # firstFound=2026-02-15, last.date=2026-02-15 -> 1 run -> 10
        row = self.by_id["delta"]
        self.assertEqual(row.runs_for_that_issue, 10)
        self.assertEqual(row.overall_found_count, 10)
        self.assertEqual(row.test_files_count, 10)
        self.assertEqual(row.last_occurrence_date, "2026-02-15")

    def test_sort_overall_found_count_desc(self):
        ids = [row.error_id for row in self.report.rows]
        self.assertEqual(ids, ["beta", "alpha", "delta", "gamma"])


class TestLegacyReportSortTiebreak(unittest.TestCase):
    def test_tie_on_overall_found_count_breaks_alphabetically(self):
        rows = [
            LegacyRow(
                error_id="zz",
                runs_for_that_issue=10,
                overall_found_count=50,
                test_files_count=5,
                last_occurrence_tool_commit="z" * 40,
                last_occurrence_date="2026-01-01",
            ),
            LegacyRow(
                error_id="aa",
                runs_for_that_issue=10,
                overall_found_count=50,
                test_files_count=5,
                last_occurrence_tool_commit="a" * 40,
                last_occurrence_date="2026-01-01",
            ),
            LegacyRow(
                error_id="mm",
                runs_for_that_issue=10,
                overall_found_count=50,
                test_files_count=5,
                last_occurrence_tool_commit="m" * 40,
                last_occurrence_date="2026-01-01",
            ),
        ]
        rows.sort(key=lambda r: (-r.overall_found_count, r.error_id))
        self.assertEqual([r.error_id for r in rows], ["aa", "mm", "zz"])


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

    def test_save_csv_header_and_first_row(self):
        with tempfile.NamedTemporaryFile("r+", suffix=".csv", delete=False, encoding="utf-8") as tmp:
            self.report.save_csv(tmp.name)
            tmp.seek(0)
            reader = list(csv.reader(tmp))
        self.assertEqual(
            reader[0],
            [
                "error_id",
                "runs_for_that_issue",
                "overall_found_count",
                "test_files_count",
                "last_occurrence_tool_commit",
                "last_occurrence_date",
            ],
        )
        # First row corresponds to beta (highest overall_found_count = 60)
        self.assertEqual(reader[1][0], "beta")
        self.assertEqual(reader[1][1], "40")
        self.assertEqual(reader[1][2], "60")
        self.assertEqual(reader[1][3], "30")
        self.assertEqual(reader[1][4], "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
        self.assertEqual(reader[1][5], "2026-01-01")
