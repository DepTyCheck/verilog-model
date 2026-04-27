import os
import unittest

from combined_report.tools_report_list import ToolsReportsList

DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")


class TestToolsReportList(unittest.TestCase):

    def test_parsing(self):
        trl = ToolsReportsList(dir_path=DATA_DIR, pattern=r"*-run-stats.json")

        self.assertEqual(len(trl.reports), 2)
