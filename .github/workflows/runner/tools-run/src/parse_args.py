import argparse


def parse_args():
    parser = argparse.ArgumentParser(description="Run analysis and simulation tests over generated modules.")

    parser.add_argument(
        "--gen-path",
        type=str,
        help="Path to generated modules",
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
        "--tool-cmd",
        type=str,
        help="Analysis tool command",
        required=True,
    )
    parser.add_argument(
        "--tool-error-regex",
        type=str,
        help="Regex for analysis errors",
        required=True,
    )
    parser.add_argument(
        "--sim-cmd",
        type=str,
        help="Simulator command",
        required=False,
    )
    parser.add_argument(
        "--sim-error-regex",
        type=str,
        help="Regex for simulation errors",
        required=False,
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

    return parser.parse_args()
