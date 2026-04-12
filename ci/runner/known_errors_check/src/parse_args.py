import argparse


def parse_args():
    parser = argparse.ArgumentParser(
        description=(
            "Regression-check known error examples against a single configured tool. "
            "All examples from found_errors/<tool-name>/*.yaml are run through the "
            "tool to verify whether the bugs are still reproducible."
        )
    )

    parser.add_argument(
        "--known-errors-dir",
        type=str,
        required=True,
        help="Path to the found_errors/ directory (contains per-tool subdirectories of YAML files)",
    )

    parser.add_argument(
        "--tool-name",
        type=str,
        required=True,
        help="Name of the tool to check (must match the subdirectory name under --known-errors-dir)",
    )

    parser.add_argument(
        "--commands-json",
        type=str,
        required=True,
        help=("JSON array of command objects with 'run' and optional 'error_regex' fields. " "The first command is used to run each example."),
    )

    parser.add_argument(
        "--language",
        type=str,
        default="sv",
        choices=["sv", "vhdl"],
        help="Source language of the tool; determines temp file suffix (default: sv)",
    )

    parser.add_argument(
        "--output",
        type=str,
        required=True,
        help="Path to write the JSON report",
    )

    parser.add_argument(
        "--error-url-prefix",
        type=str,
        required=False,
        default=None,
        help="URL prefix for error links in the Markdown summary table (error ID will be appended)",
    )

    return parser.parse_args()
