# ci/runner/mds_report/parse_args.py
import argparse


def parse_args():
    parser = argparse.ArgumentParser(description="Generate an interactive MDS distance diagram from per-file JSON.")
    parser.add_argument("--per-file-input", type=str, required=True, help="Per-file JSON written by runner.main")
    parser.add_argument("--ignored-errors-dir", type=str, required=True, help="Path to the tool's known-errors directory")
    parser.add_argument("--tool-name", type=str, required=True)
    parser.add_argument("--job-link", type=str, required=True)
    parser.add_argument("--output", type=str, default="error_distances.html", help="Output HTML file path")
    return parser.parse_args()
