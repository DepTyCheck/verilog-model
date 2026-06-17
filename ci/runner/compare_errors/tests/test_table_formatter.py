import unittest

from compare_errors.compare_errors import ErrorPercentageDelta
from compare_errors.table_formatter import format_table


class TestTableFormatter(unittest.TestCase):
    def _make_deltas(self):
        return [
            ErrorPercentageDelta("t_dll_api_cc_ivl_nexus_s", historical_pct=3.76, current_pct=32.0, master_pct=10.0),
            ErrorPercentageDelta("new_error_id", historical_pct=0.0, current_pct=9.0, master_pct=0.0),
            ErrorPercentageDelta("cannot_be_driven_with_non_default_strength", historical_pct=4.51, current_pct=3.0, master_pct=4.0),
            ErrorPercentageDelta("stale_error_not_in_current_run", historical_pct=1.88, current_pct=0.0, master_pct=2.0),
        ]

    def test_returns_string(self):
        self.assertIsInstance(format_table(self._make_deltas()), str)

    def test_contains_all_headers(self):
        table = format_table(self._make_deltas())
        for header in ["Error ID", "Historical %", "Delta cur-hist", "Master %", "Delta cur-master", "Current %"]:
            self.assertIn(header, table)

    def test_contains_all_error_ids(self):
        table = format_table(self._make_deltas())
        self.assertIn("t_dll_api_cc_ivl_nexus_s", table)
        self.assertIn("new_error_id", table)
        self.assertIn("cannot_be_driven_with_non_default_strength", table)
        self.assertIn("stale_error_not_in_current_run", table)

    def test_positive_delta_has_green_square_and_plus(self):
        # master_pct (8.0) != historical_pct (5.0) so the two delta columns differ:
        # delta_pct = +5.00 (cur-hist), delta_vs_master = +2.00 (cur-master).
        table = format_table([ErrorPercentageDelta("foo", historical_pct=5.0, current_pct=10.0, master_pct=8.0)])
        self.assertIn("🟩 +5.00%", table)
        self.assertIn("🟩 +2.00%", table)

    def test_negative_delta_has_red_square(self):
        table = format_table([ErrorPercentageDelta("baz", historical_pct=10.0, current_pct=7.0, master_pct=7.0)])
        self.assertIn("🟥 -3.00%", table)

    def test_zero_delta_has_no_square(self):
        table = format_table([ErrorPercentageDelta("bar", historical_pct=5.0, current_pct=5.0, master_pct=5.0)])
        self.assertIn("0.00%", table)
        self.assertNotIn("🟩", table)
        self.assertNotIn("🟥", table)
        self.assertNotIn("+0.00%", table)

    def test_near_zero_negative_delta_shows_no_red_square(self):
        # delta_pct = current - historical = 1.996 - 2.0 = -0.004 -> rounds to 0.00, no square.
        table = format_table([ErrorPercentageDelta("foo", historical_pct=2.0, current_pct=1.996, master_pct=1.996)])
        self.assertIn("0.00%", table)
        self.assertNotIn("🟥", table)
        self.assertNotIn("-0.00%", table)

    def test_master_pct_rendered(self):
        # Master % (42.50) must render in its own column, left of Current % (2.00).
        table = format_table([ErrorPercentageDelta("foo", historical_pct=1.0, current_pct=2.0, master_pct=42.5)])
        self.assertIn("42.50%", table)
        self.assertLess(table.index("42.50%"), table.index("2.00%"))

    def test_empty_deltas(self):
        self.assertIn("No errors", format_table([]))

    def test_table_has_separator_line(self):
        self.assertIn("|-", format_table(self._make_deltas()))

    def test_rows_sorted_by_abs_delta_vs_master(self):
        deltas = [
            ErrorPercentageDelta("small", historical_pct=5.0, current_pct=6.0, master_pct=5.5),
            ErrorPercentageDelta("large_pos", historical_pct=0.0, current_pct=32.0, master_pct=2.0),
            ErrorPercentageDelta("large_neg", historical_pct=20.0, current_pct=5.0, master_pct=24.0),
        ]
        table = format_table(deltas)
        # delta_vs_master: small=0.5, large_pos=30.0, large_neg=-19.0
        self.assertLess(table.index("large_pos"), table.index("large_neg"))
        self.assertLess(table.index("large_neg"), table.index("small"))

    def test_percentages_use_two_decimal_places(self):
        table = format_table([ErrorPercentageDelta("foo", historical_pct=3.759398, current_pct=12.5, master_pct=0.0)])
        self.assertIn("3.76%", table)
        self.assertIn("12.50%", table)

    def test_error_id_plain_without_prefix(self):
        table = format_table([ErrorPercentageDelta("some_error", historical_pct=1.0, current_pct=2.0, master_pct=1.0)])
        self.assertIn("some_error", table)
        self.assertNotIn("](", table)

    def test_error_id_is_markdown_link_with_prefix(self):
        table = format_table(
            [ErrorPercentageDelta("some_error", historical_pct=1.0, current_pct=2.0, master_pct=1.0)],
            error_url_prefix="https://org.github.io/repo/error",
        )
        self.assertIn("[some_error](https://org.github.io/repo/error/some_error)", table)

    def test_known_errors_column_present_when_provided(self):
        table = format_table(
            [ErrorPercentageDelta("foo", historical_pct=1.0, current_pct=2.0, master_pct=1.0)],
            known_errors={"foo": True},
        )
        self.assertIn("Reproduced", table)

    def test_known_errors_column_absent_when_not_provided(self):
        table = format_table([ErrorPercentageDelta("foo", historical_pct=1.0, current_pct=2.0, master_pct=1.0)])
        self.assertNotIn("Reproduced", table)

    def test_known_error_true_shows_checkmark(self):
        table = format_table(
            [ErrorPercentageDelta("foo", historical_pct=1.0, current_pct=2.0, master_pct=1.0)],
            known_errors={"foo": True},
        )
        self.assertIn("✅", table)

    def test_known_error_false_shows_cross(self):
        table = format_table(
            [ErrorPercentageDelta("foo", historical_pct=1.0, current_pct=2.0, master_pct=1.0)],
            known_errors={"foo": False},
        )
        self.assertIn("❌", table)

    def test_known_error_missing_id_shows_cross(self):
        table = format_table(
            [ErrorPercentageDelta("foo", historical_pct=1.0, current_pct=2.0, master_pct=1.0)],
            known_errors={},
        )
        self.assertIn("❌", table)


if __name__ == "__main__":
    unittest.main()
