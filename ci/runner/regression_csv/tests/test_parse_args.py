import unittest
from unittest.mock import patch

from regression_csv.parse_args import parse_args


class TestParseArgs(unittest.TestCase):
    def test_all_required_args(self):
        argv = [
            "prog",
            "--regression-results-dir",
            "regression-results",
            "--known-errors-dir",
            "dataset-clone/found_issues",
            "--out-csv",
            "dataset-clone/latest_regression_test.csv",
        ]
        with patch("sys.argv", argv):
            args = parse_args()
        self.assertEqual(args.regression_results_dir, "regression-results")
        self.assertEqual(args.known_errors_dir, "dataset-clone/found_issues")
        self.assertEqual(args.out_csv, "dataset-clone/latest_regression_test.csv")

    def test_missing_required_arg_exits(self):
        argv = ["prog", "--regression-results-dir", "x"]
        with patch("sys.argv", argv):
            with self.assertRaises(SystemExit):
                parse_args()


if __name__ == "__main__":
    unittest.main()
