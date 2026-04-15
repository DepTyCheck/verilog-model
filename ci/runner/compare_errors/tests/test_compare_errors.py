import json
import os
import tempfile
import unittest

from combined_report.percentages import occurrence_pct
from combined_report.previous_report import PreviousReport
from combined_report.tools_report_list import ToolsReportsList
from compare_errors.compare_errors import ErrorPercentageDelta, ErrorsComparison
from compare_errors.main import _load_known_errors

DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")
PREVIOUS_REPORT = os.path.join(DATA_DIR, "previous_report.json")

# previous_report.json has runs: [{amount: 256}, {amount: 10}] → total 266
HISTORICAL_TOTAL = 266
# tool-a: new_error_id=7, t_dll=32; tool-b: cannot_be_driven=3, new_error_id=2
TESTS_NUMBER = 100


class TestErrorPercentageDelta(unittest.TestCase):
    def test_positive_delta(self):
        d = ErrorPercentageDelta(error_id="foo", historical_pct=5.0, current_pct=12.5)
        self.assertAlmostEqual(d.delta_pct, 7.5)

    def test_zero_delta(self):
        d = ErrorPercentageDelta(error_id="foo", historical_pct=10.0, current_pct=10.0)
        self.assertAlmostEqual(d.delta_pct, 0.0)

    def test_negative_delta(self):
        d = ErrorPercentageDelta(error_id="foo", historical_pct=20.0, current_pct=5.0)
        self.assertAlmostEqual(d.delta_pct, -15.0)

    def test_new_error(self):
        d = ErrorPercentageDelta(error_id="new", historical_pct=0.0, current_pct=7.0)
        self.assertAlmostEqual(d.delta_pct, 7.0)


class TestErrorsComparison(unittest.TestCase):
    def setUp(self):
        previous = PreviousReport(PREVIOUS_REPORT)
        tools = ToolsReportsList(dir_path=DATA_DIR, pattern="*-run-stats.json")
        self.comparison = ErrorsComparison(
            previous_report=previous,
            tools_reports_list=tools,
            tests_number=TESTS_NUMBER,
        )

    def test_current_run_counts_sums_across_tools(self):
        counts = self.comparison.current_run_counts()
        # tool-a: new_error_id=7, tool-b: new_error_id=2 => total 9
        self.assertEqual(counts["new_error_id"], 9)

    def test_current_run_counts_single_tool(self):
        counts = self.comparison.current_run_counts()
        # only tool-a has t_dll_api_cc_ivl_nexus_s with overall=32
        self.assertEqual(counts["t_dll_api_cc_ivl_nexus_s"], 32)

    def test_compare_includes_all_error_ids(self):
        deltas = self.comparison.compare()
        ids = {d.error_id for d in deltas}
        self.assertIn("t_dll_api_cc_ivl_nexus_s", ids)
        self.assertIn("cannot_be_driven_with_non_default_strength", ids)
        self.assertIn("new_error_id", ids)
        self.assertIn("stale_error_not_in_current_run", ids)

    def test_historical_pct_for_known_error(self):
        deltas = self.comparison.compare()
        entry = next(d for d in deltas if d.error_id == "t_dll_api_cc_ivl_nexus_s")
        # previous overall=10, historical_total=266
        self.assertAlmostEqual(entry.historical_pct, occurrence_pct(10, HISTORICAL_TOTAL))

    def test_current_pct_for_known_error(self):
        deltas = self.comparison.compare()
        entry = next(d for d in deltas if d.error_id == "t_dll_api_cc_ivl_nexus_s")
        # current run overall=32, tests_number=100
        self.assertAlmostEqual(entry.current_pct, occurrence_pct(32, TESTS_NUMBER))

    def test_new_error_historical_pct_is_zero(self):
        deltas = self.comparison.compare()
        entry = next(d for d in deltas if d.error_id == "new_error_id")
        self.assertAlmostEqual(entry.historical_pct, 0.0)
        self.assertAlmostEqual(entry.current_pct, occurrence_pct(9, TESTS_NUMBER))

    def test_stale_error_current_pct_is_zero(self):
        deltas = self.comparison.compare()
        entry = next(d for d in deltas if d.error_id == "stale_error_not_in_current_run")
        self.assertAlmostEqual(entry.current_pct, 0.0)
        self.assertAlmostEqual(entry.historical_pct, occurrence_pct(5, HISTORICAL_TOTAL))

    def test_compare_sorted_by_abs_delta_pct_descending(self):
        deltas = self.comparison.compare()
        for i in range(len(deltas) - 1):
            self.assertGreaterEqual(abs(deltas[i].delta_pct), abs(deltas[i + 1].delta_pct))


def _write_report(tmp: str, filename: str, content: dict) -> None:
    with open(os.path.join(tmp, filename), "w", encoding="utf-8") as f:
        json.dump(content, f)


class TestLoadKnownErrors(unittest.TestCase):

    def test_at_least_one_true_means_reproduced(self):
        with tempfile.TemporaryDirectory() as tmp:
            _write_report(
                tmp,
                "tool_a.json",
                {
                    "err_x": {"example_1-minified": False, "example_1-full": True, "example_2-minified": False},
                },
            )
            result = _load_known_errors(tmp)
        self.assertTrue(result["err_x"])

    def test_all_false_means_not_reproduced(self):
        with tempfile.TemporaryDirectory() as tmp:
            _write_report(
                tmp,
                "tool_a.json",
                {
                    "err_x": {"example_1-minified": False, "example_1-full": False, "example_2-minified": False, "example_2-full": False},
                },
            )
            result = _load_known_errors(tmp)
        self.assertFalse(result["err_x"])

    def test_multiple_files_reproduced_in_any_one_means_true(self):
        with tempfile.TemporaryDirectory() as tmp:
            _write_report(tmp, "tool_a.json", {"err_x": {"example_1-minified": False}})
            _write_report(tmp, "tool_b.json", {"err_x": {"example_1-minified": True}})
            result = _load_known_errors(tmp)
        self.assertTrue(result["err_x"])

    def test_multiple_files_all_false_means_not_reproduced(self):
        with tempfile.TemporaryDirectory() as tmp:
            _write_report(tmp, "tool_a.json", {"err_x": {"example_1-minified": False}})
            _write_report(tmp, "tool_b.json", {"err_x": {"example_1-full": False}})
            result = _load_known_errors(tmp)
        self.assertFalse(result["err_x"])

    def test_error_absent_from_all_reports_is_not_in_result(self):
        with tempfile.TemporaryDirectory() as tmp:
            _write_report(tmp, "tool_a.json", {"err_a": {"example_1-minified": True}})
            result = _load_known_errors(tmp)
        self.assertNotIn("err_b", result)

    def test_empty_dir_returns_empty_dict(self):
        with tempfile.TemporaryDirectory() as tmp:
            result = _load_known_errors(tmp)
        self.assertEqual(result, {})
