import json
import os
import unittest

from combined_report.tools_report import ToolsReport

DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")


class TestToolsReport(unittest.TestCase):

    def test_parsing(self):
        with open(os.path.join(DATA_DIR, "some-tool-run-stats.json"), "r", encoding="utf-8") as f:
            data_dict = json.load(f)

        p = ToolsReport(data_dict)

        self.assertEqual(p.errors[0].error_id, "t_dll_api_cc_ivl_nexus_s")
        self.assertEqual(p.errors[0].overall, 32)
        self.assertEqual(p.commit, "54c4f9f4")
        self.assertEqual(p.date, "2025-11-04T12:38:39.969316")
