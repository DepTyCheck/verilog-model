# ci/runner/regression_input/parse_args.py
import argparse


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Materialise regression example files from known-errors YAMLs.")
    p.add_argument("--known-errors-dir", type=str, required=True)
    p.add_argument("--language", type=str, required=True, help="sv | vhdl")
    p.add_argument("--language-config", type=str, required=True, help="Path to languages.yaml")
    p.add_argument("--out-dir", type=str, required=True)
    return p.parse_args()
