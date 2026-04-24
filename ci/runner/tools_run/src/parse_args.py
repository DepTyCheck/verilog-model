import argparse


def parse_args():
    parser = argparse.ArgumentParser(description="Run analysis and simulation tests over generated modules.")
    parser.add_argument("--gen-path", type=str, required=True, help="Path to generated files")
    parser.add_argument("--file-pattern", type=str, required=True, help="Glob pattern to find files (e.g. '*.sv')")
    parser.add_argument("--job-link", type=str, default=None, help="Job URL for MDS diagram (passed to mds_report, not used directly)")
    parser.add_argument("--tool-name", type=str, required=True)
    parser.add_argument("--commands-json", type=str, required=True, help="JSON array of command objects with 'run' and optional 'error_regex'")
    parser.add_argument("--ignored-errors-dir", type=str, required=True, help="Path to directory with ignored error YAML files")
    parser.add_argument("--unknown-errors-output", type=str, default=None, help="Path to write unknown_errors.json for mds_report")
    parser.add_argument("--extra-ignored-regexes", type=str, nargs="*", default=[])
    parser.add_argument("--run-statistics-output", type=str, required=True, help="Path to save run statistics JSON")
    parser.add_argument("--commit", type=str, required=True)
    parser.add_argument("--assets", type=str, nargs="*", default=[])
    parser.add_argument("--tool-version", type=str, default="", help="Tool version string")
    parser.add_argument("--model-commit", type=str, default="", help="verilog-model repo commit SHA")
    parser.add_argument("--per-file-output", type=str, default=None, help="Path to save per-file results JSON")
    return parser.parse_args()
