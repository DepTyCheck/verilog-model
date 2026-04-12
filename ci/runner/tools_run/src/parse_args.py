import argparse


def parse_args():
    parser = argparse.ArgumentParser(description="Run analysis and simulation tests over generated modules.")

    parser.add_argument(
        "--gen-path",
        type=str,
        help="Path to generated files",
        required=True,
    )
    parser.add_argument(
        "--file-pattern",
        type=str,
        help="Pattern to find files",
        required=True,
    )
    parser.add_argument(
        "--job-link",
        type=str,
        help="Path to generated modules",
        required=True,
    )
    parser.add_argument(
        "--tool-name",
        type=str,
        help="Analysis tool name",
        required=True,
    )
    parser.add_argument(
        "--commands-json",
        type=str,
        help="JSON array of command objects with 'run' and optional 'error_regex' fields",
        required=True,
    )
    parser.add_argument(
        "--ignored-errors-dir",
        type=str,
        help="Path to directory with ignored error YAML files",
        required=True,
    )
    parser.add_argument(
        "--error-distances-output",
        type=str,
        help="Path to save error distances interactive plot",
        required=True,
    )
    parser.add_argument(
        "--extra-ignored-regexes",
        type=str,
        nargs="*",
        default=[],
        help="Additional regexes to ignore (can be specified multiple times)",
    )
    parser.add_argument(
        "--run-statistics-output",
        type=str,
        help="Path to save run statistics",
        required=True,
    )
    parser.add_argument(
        "--commit",
        type=str,
        help="Commit",
        required=True,
    )
    parser.add_argument(
        "--assets",
        type=str,
        nargs="*",
        default=[],
        help="Assets to copy to the working directory",
    )

    return parser.parse_args()
