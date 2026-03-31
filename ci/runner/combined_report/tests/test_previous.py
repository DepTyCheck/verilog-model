import os
import unittest

from combined_report.previous_report import PreviousReport

DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")


class TestPreviousReport(unittest.TestCase):

    def test_parsing(self):
        p = PreviousReport(os.path.join(DATA_DIR, "previous_report.json"))

        self.assertEqual(p.errors["t_dll_api_cc_ivl_nexus_s"].overall, 10)
        self.assertEqual(p.runs[0].amount, 256)
