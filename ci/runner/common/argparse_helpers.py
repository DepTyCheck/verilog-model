import argparse


def add_error_url_prefix_arg(parser: argparse.ArgumentParser, help_text: str) -> None:
    parser.add_argument(
        "--error-url-prefix",
        type=str,
        required=False,
        default=None,
        help=help_text,
    )
