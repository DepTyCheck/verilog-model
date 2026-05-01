import unittest

from dataset_stats.combined_row import CombinedRow


class TestCombinedRow(unittest.TestCase):
    def test_csv_header(self):
        self.assertEqual(
            CombinedRow.csv_header(),
            [
                "error_id",
                "runs_for_that_issue",
                "overall_found_count",
                "test_files_count",
                "last_occurrence_tool_commit",
                "last_occurrence_date",
                "last_model_commit",
            ],
        )

    def test_to_csv_fields_includes_model_commit(self):
        row = CombinedRow(
            error_id="alpha",
            runs_for_that_issue=10,
            overall_found_count=5,
            test_files_count=3,
            last_occurrence_tool_commit="abc",
            last_occurrence_date="2026-04-29",
            last_model_commit="def",
        )
        self.assertEqual(
            row.to_csv_fields(),
            ["alpha", "10", "5", "3", "abc", "2026-04-29", "def"],
        )

    def test_to_csv_fields_empty_model_commit(self):
        row = CombinedRow(
            error_id="legacy_only",
            runs_for_that_issue=1,
            overall_found_count=1,
            test_files_count=1,
            last_occurrence_tool_commit="abc",
            last_occurrence_date="2025-01-01",
            last_model_commit="",
        )
        self.assertEqual(row.to_csv_fields()[-1], "")


if __name__ == "__main__":
    unittest.main()
