import os
import unittest

from combined_report.combined_report import CombinedReport
from combined_report.previous_report import PreviousReport
from combined_report.tools_report_list import ToolsReportsList

DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")


class TestCombinedReport(unittest.TestCase):
    def setUp(self):
        p = PreviousReport(os.path.join(DATA_DIR, "previous_report.json"))
        trl = ToolsReportsList(dir_path=DATA_DIR, pattern=r"*-run-stats.json")
        self.tests_number = 1337

        self.combined_report = CombinedReport(previous_report=p, tools_reports_list=trl, tests_number=self.tests_number)

    def test_runs(self):
        runs = self.combined_report.combined_runs()
        last_run = runs[-1]

        self.assertEqual(len(runs), 3)
        self.assertIsInstance(last_run.date, str)
        self.assertGreater(len(last_run.date), 0)
        self.assertEqual(last_run.amount, self.tests_number)

    def test_errors(self):
        data = self.combined_report.combined_errors()

        self.assertIn("t_dll_api_cc_ivl_nexus_s", list(data.keys()))
        self.assertIn("cannot_be_driven_with_non_default_strength", list(data.keys()))
        self.assertIn("new_error_id", list(data.keys()))

        self.assertEqual(data["t_dll_api_cc_ivl_nexus_s"].overall, 42)
        self.assertEqual(
            data["cannot_be_driven_with_non_default_strength"].test_paths_count,
            420,
        )
        self.assertEqual(
            data["cannot_be_driven_with_non_default_strength"].last.commit,
            "abc12345",
        )
        self.assertIsInstance(
            data["cannot_be_driven_with_non_default_strength"].last.date,
            str,
        )
