import unittest

from legacy_stats.legacy_row import LegacyRow


class TestLegacyRow(unittest.TestCase):
    def test_to_csv_fields_returns_strings_in_schema_order(self):
        row = LegacyRow(
            error_id="some_error",
            runs_for_that_issue=42,
            overall_found_count=9624,
            test_files_count=4448,
            last_occurrence_tool_commit="8a24897c13e0b15e76bc49a038986b7d2728fb2a",
            last_occurrence_date="2026-01-13",
        )
        self.assertEqual(
            row.to_csv_fields(),
            [
                "some_error",
                "42",
                "9624",
                "4448",
                "8a24897c13e0b15e76bc49a038986b7d2728fb2a",
                "2026-01-13",
            ],
        )

    def test_header_constant_matches_schema(self):
        self.assertEqual(
            LegacyRow.csv_header(),
            [
                "error_id",
                "runs_for_that_issue",
                "overall_found_count",
                "test_files_count",
                "last_occurrence_tool_commit",
                "last_occurrence_date",
            ],
        )
