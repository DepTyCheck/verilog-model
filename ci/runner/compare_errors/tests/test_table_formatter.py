import unittest

from compare_errors.compare_errors import ErrorPercentageDelta
from compare_errors.table_formatter import format_table


class TestTableFormatter(unittest.TestCase):
    def _make_deltas(self):
        return [
            ErrorPercentageDelta("t_dll_api_cc_ivl_nexus_s", historical_pct=3.76, current_pct=32.0),
            ErrorPercentageDelta("new_error_id", historical_pct=0.0, current_pct=9.0),
            ErrorPercentageDelta(
                "cannot_be_driven_with_non_default_strength",
                historical_pct=4.51,
                current_pct=3.0,
            ),
            ErrorPercentageDelta("stale_error_not_in_current_run", historical_pct=1.88, current_pct=0.0),
        ]

    def test_returns_string(self):
        self.assertIsInstance(format_table(self._make_deltas()), str)

    def test_contains_header(self):
        table = format_table(self._make_deltas())
        self.assertIn("Error ID", table)
        self.assertIn("Historical %", table)
        self.assertIn("Current %", table)
        self.assertIn("Delta %", table)

    def test_contains_all_error_ids(self):
        table = format_table(self._make_deltas())
        self.assertIn("t_dll_api_cc_ivl_nexus_s", table)
        self.assertIn("new_error_id", table)
        self.assertIn("cannot_be_driven_with_non_default_strength", table)
        self.assertIn("stale_error_not_in_current_run", table)

    def test_positive_delta_has_plus_sign(self):
        table = format_table([ErrorPercentageDelta("foo", historical_pct=5.0, current_pct=10.0)])
        self.assertIn("+5.00%", table)

    def test_zero_delta_formatted(self):
        table = format_table([ErrorPercentageDelta("bar", historical_pct=5.0, current_pct=5.0)])
        self.assertIn("0.00%", table)
        self.assertNotIn("+0.00%", table)

    def test_negative_delta_formatted(self):
        table = format_table([ErrorPercentageDelta("baz", historical_pct=10.0, current_pct=7.0)])
        self.assertIn("-3.00%", table)

    def test_empty_deltas(self):
        self.assertIn("No errors", format_table([]))

    def test_table_has_separator_line(self):
        self.assertIn("|-", format_table(self._make_deltas()))

    def test_rows_appear_in_sorted_order_by_abs_delta(self):
        deltas = [
            ErrorPercentageDelta("small_positive", historical_pct=5.0, current_pct=6.0),
            ErrorPercentageDelta("large_positive", historical_pct=0.0, current_pct=32.0),
            ErrorPercentageDelta("large_negative", historical_pct=20.0, current_pct=5.0),
        ]
        table = format_table(deltas)
        # sorted by abs(delta): large_positive(32) > large_negative(15) > small_positive(1)
        self.assertLess(table.index("large_positive"), table.index("large_negative"))
        self.assertLess(table.index("large_negative"), table.index("small_positive"))

    def test_percentages_use_two_decimal_places(self):
        table = format_table([ErrorPercentageDelta("foo", historical_pct=3.759398, current_pct=12.5)])
        self.assertIn("3.76%", table)
        self.assertIn("12.50%", table)

    def test_error_id_plain_without_prefix(self):
        table = format_table([ErrorPercentageDelta("some_error", historical_pct=1.0, current_pct=2.0)])
        self.assertIn("some_error", table)
        self.assertNotIn("](", table)

    def test_error_id_is_markdown_link_with_prefix(self):
        table = format_table(
            [ErrorPercentageDelta("some_error", historical_pct=1.0, current_pct=2.0)],
            error_url_prefix="https://org.github.io/repo/error",
        )
        self.assertIn("[some_error](https://org.github.io/repo/error/some_error)", table)
