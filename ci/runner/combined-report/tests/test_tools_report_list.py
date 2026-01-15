import unittest

from src.tools_report_list import ToolsReportsList


class TestToolsReportList(unittest.TestCase):

    def test_parsing(self):
        trl = ToolsReportsList(dir_path="./tests/data", pattern=r"*-run-stats.json")

        self.assertEqual(len(trl.reports), 2)
