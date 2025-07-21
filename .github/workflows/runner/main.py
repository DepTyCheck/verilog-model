#!/usr/bin/env python3

import sys
import argparse
from pathlib import Path
from ignored_errors_list import IgnoredErrorsList, UnexpectedErrorText, FoundMatch
from collections import Counter
from found_error import (
    FoundError,
    compute_ncd_for_errors,
    plot_error_distances_mds,
)
from run_test import make_command, run_test
from utils import print_pretty


def parse_args():
    parser = argparse.ArgumentParser(description="Run analysis and simulation tests over generated modules.")

    parser.add_argument(
        "--gen-path",
        type=str,
        help="Path to generated modules",
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
        "--error-distances-img",
        type=str,
        help="Path to save error distances image",
        required=True,
    )
    parser.add_argument(
        "--extra-ignored-regexes",
        type=str,
        nargs="*",
        default=[],
        help="Additional regexes to ignore (can be specified multiple times)",
    )

    return parser.parse_args()


def run(
    raw_cmd: str,
    tool_error_regex: str,
    file_path_str: str,
    file_content: str,
    ignored_errors: IgnoredErrorsList,
    all_found_errors: list[FoundError],
) -> tuple[bool, list[UnexpectedErrorText], list[FoundMatch]]:
    real_cmd = make_command(
        cmd=raw_cmd,
        file_path=file_path_str,
        file_content=file_content,
    )
    cmd_res, unexpected_errors, found_matches = run_test(
        cmd=real_cmd,
        file_content=file_content,
        file_path=file_path_str,
        error_regex=tool_error_regex,
        ignored_errors=ignored_errors,
    )
    for err in unexpected_errors:
        all_found_errors.append(FoundError(err, file_path_str))

    return cmd_res, unexpected_errors, found_matches


def main() -> None:
    args = parse_args()
    gen_path = args.gen_path
    failed_files: list[str] = []
    ignored_errors = IgnoredErrorsList(
        args.ignored_errors_dir,
        regex_list=args.extra_ignored_regexes,
    )
    # Strip trailing newlines from regex arguments
    args.tool_error_regex = args.tool_error_regex.rstrip("\n")
    if args.sim_error_regex:
        args.sim_error_regex = args.sim_error_regex.rstrip("\n")
    stats = Counter()
    all_found_errors: list[FoundError] = []

    for file_path in Path(gen_path).glob("*.sv"):
        file_path_str = str(file_path)
        with open(file_path, "r", encoding="utf-8") as file:
            file_content = file.read()

        # Run analysis
        cmd_res, cmd_unexpected_errors, cmd_found_matches = run(
            raw_cmd=args.tool_cmd,
            tool_error_regex=args.tool_error_regex,
            file_path_str=file_path_str,
            file_content=file_content,
            ignored_errors=ignored_errors,
            all_found_errors=all_found_errors,
        )

        sim_res = True
        sim_unexpected_errors: list[UnexpectedErrorText] = []

        # Run simulation if configured
        if cmd_res and args.sim_cmd:
            sim_res, sim_unexpected_errors, sim_found_matches = run(
                raw_cmd=args.sim_cmd,
                tool_error_regex=args.sim_error_regex,
                file_path_str=file_path_str,
                file_content=file_content,
                ignored_errors=ignored_errors,
                all_found_errors=all_found_errors,
            )

        if cmd_res and sim_res:
            stats["clean"] += 1
        elif len(cmd_unexpected_errors + sim_unexpected_errors) == 0:
            stats["handled_errors"] += 1
        else:
            stats["failed"] += 1
            failed_files.append(file_path_str)

    if all_found_errors:
        print_pretty([f"Error in {error.file_name}: {error.text}" for error in all_found_errors])

        # Compute NCD for all found errors
        ncd_results = compute_ncd_for_errors(
            all_found_errors,
            ".github/workflows/runner/ncd-xz.sh",
        )
        plot_error_distances_mds(
            all_found_errors,
            ncd_results,
            output_path=args.error_distances_img,
        )

    print_pretty(
        [
            "Test Statistics:",
            f"Clean tests:   {stats['clean']}",
            f"Known issues:  {stats['handled_errors']}",
            f"Failed tests:  {stats['failed']}",
            f"Total tests:   {sum(stats.values())}",
        ]
    )

    if failed_files:
        print_pretty(
            [
                f"  Total failed tests: {len(failed_files)}",
                "  Failed files:",
                *[f"  - {failed_file}" for failed_file in failed_files],
            ]
        )
        sys.exit(1)
    else:
        print_pretty(["  All tests passed successfully."])


if __name__ == "__main__":
    main()
