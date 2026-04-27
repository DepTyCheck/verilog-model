import unittest

from legacy_stats.legacy_row import LegacyRow


class TestLegacyRow(unittest.TestCase):
    def test_to_csv_fields_two_decimals_and_full_commit(self):
        row = LegacyRow(
            error_id="some_error",
            occurrence_pct=69.831,
            files_pct=32.2,
            avg_errors_per_file=2.0,
            last_commit="8a24897c13e0b15e76bc49a038986b7d2728fb2a",
            last_date="13.01.2026",
        )
        self.assertEqual(
            row.to_csv_fields(),
            [
                "some_error",
                "69.83",
                "32.20",
                "2.00",
                "8a24897c13e0b15e76bc49a038986b7d2728fb2a",
                "13.01.2026",
            ],
        )

    def test_header_constant_matches_schema(self):
        self.assertEqual(
            LegacyRow.csv_header(),
            ["error_id", "occurrence_pct", "files_pct", "avg_errors_per_file", "last_commit", "last_date"],
        )
