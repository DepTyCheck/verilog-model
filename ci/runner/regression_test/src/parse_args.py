import argparse

from common.argparse_helpers import add_error_url_prefix_arg


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
        help="Source language of the tool (default: sv)",
    )

    parser.add_argument(
        "--language-config",
        type=str,
        required=True,
        help="Path to a YAML file mapping language names to file extensions (e.g. sv: .sv).",
    )

    parser.add_argument(
        "--output",
        type=str,
        required=True,
        help="Path to write the JSON report",
    )

    add_error_url_prefix_arg(parser, "URL prefix for error links in the Markdown summary table (error ID will be appended)")

    parser.add_argument(
        "--extra-ignored-regexes",
        type=str,
        nargs="*",
        default=[],
        help="Additional regexes to ignore beyond the known error YAML files (can be specified multiple times)",
    )

    parser.add_argument(
        "--unknown-errors-output",
        type=str,
        required=False,
        default=None,
        help="Path to write unknown_errors.json for mds_report (optional)",
    )

    parser.add_argument(
        "--assets",
        type=str,
        nargs="*",
        default=[],
        help="Paths to asset files/directories to copy into each temp run directory",
    )

    return parser.parse_args()
