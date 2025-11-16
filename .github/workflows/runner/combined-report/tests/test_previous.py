import unittest

from src.previous_report import PreviousReport


class TestPreviousReport(unittest.TestCase):

    def test_parsing(self):
        p = PreviousReport("./tests/data/previous_report.json")

        self.assertEqual(p.errors["t_dll_api_cc_ivl_nexus_s"].overall, 10)
        self.assertEqual(p.runs[0].amount, 256)
