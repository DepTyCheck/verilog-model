import argparse


def parse_args():
    parser = argparse.ArgumentParser(description="Generate an interactive MDS distance diagram from unknown errors.")
    parser.add_argument(
        "--unknown-errors-input",
        type=str,
        required=True,
        help="Path to unknown_errors.json written by tools_run or regression_test",
    )
    parser.add_argument(
        "--ignored-errors-dir",
        type=str,
        required=True,
        help="Path to the tool's known-errors directory (for distance calculation)",
    )
    parser.add_argument(
        "--tool-name",
        type=str,
        required=True,
    )
    parser.add_argument(
        "--job-link",
        type=str,
        required=True,
    )
    parser.add_argument(
        "--output",
        type=str,
        default="error_distances.html",
        help="Output HTML file path",
    )
    return parser.parse_args()
