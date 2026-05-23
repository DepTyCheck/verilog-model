import argparse


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description="Write latest_regression_test.csv from regression per-file JSON artifacts.",
    )
    p.add_argument("--regression-results-dir", type=str, required=True, help="Directory containing regression-<tool>-per-file.json artifacts")
    p.add_argument("--known-errors-dir", type=str, required=True, help="Root of found_issues/ on the dataset branch clone")
    p.add_argument("--out-csv", type=str, required=True, help="Path to write the CSV (overwritten if it exists)")
    return p.parse_args()
