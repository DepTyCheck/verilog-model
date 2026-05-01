# ci/runner/regression_analyze/parse_args.py
import argparse


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Build a regression reproducibility markdown table from per-file JSON.")
    p.add_argument("--per-file-input", type=str, required=True)
    p.add_argument("--known-errors-dir", type=str, required=True)
    p.add_argument("--language", type=str, required=True, help="sv | vhdl — filter expected map by language")
    p.add_argument("--tool-name", type=str, required=True, help="Filter expected map to errors registered for this tool")
    p.add_argument("--error-url-prefix", type=str, default=None)
    return p.parse_args()
