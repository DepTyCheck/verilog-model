# ci/runner/runner/parse_args.py
import argparse


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Run a tool over a directory of source files and emit per-file JSON.")
    p.add_argument("--input-dir", type=str, required=True)
    p.add_argument("--file-pattern", type=str, required=True, help="Glob (e.g. '*.sv') applied inside --input-dir")
    p.add_argument("--tool-name", type=str, required=True)
    p.add_argument("--tool-version", type=str, default="")
    p.add_argument("--tool-commit", type=str, default="")
    p.add_argument("--model-commit", type=str, default="")
    p.add_argument("--commands-json", type=str, required=True, help="JSON array of {run, error_regex} command objects")
    p.add_argument("--ignored-errors-dir", type=str, required=True)
    p.add_argument("--assets", type=str, nargs="*", default=[])
    p.add_argument("--extra-ignored-regexes", type=str, nargs="*", default=[])
    p.add_argument("--output", type=str, required=True, help="Output per-file JSON path")
    return p.parse_args()
