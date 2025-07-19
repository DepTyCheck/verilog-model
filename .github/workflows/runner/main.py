#!/usr/bin/env python3

import sys
import argparse
from pathlib import Path
from ignored_errors_list import IgnoredErrorsList
from collections import Counter
from found_error import FoundError, compute_ncd_for_errors, plot_error_distances_mds
from test_utils import make_command, run_test
from utils import print_pretty

def parse_args():
    parser = argparse.ArgumentParser(
        description="Run analysis and simulation tests over generated modules."
    )

    parser.add_argument('--gen-path', type=str, help="Path to generated modules", required=True)
    parser.add_argument('--tool-cmd', type=str, help="Analysis tool command", required=True)
    parser.add_argument('--tool-error-regex', type=str, help="Regex for analysis errors", required=True)

    parser.add_argument('--sim-cmd', type=str, help="Simulator command", required=False)
    parser.add_argument('--sim-error-regex', type=str, help="Regex for simulation errors", required=False)

    parser.add_argument('--ignored-errors-dir', type=str, help="Path to directory with ignored error YAML files", required=True)
    parser.add_argument('--error-distances-img', type=str, help="Path to save error distances image", required=True)
    parser.add_argument('--extra-ignored-regexes', type=str, nargs='*', default=[], help="Additional regexes to ignore (can be specified multiple times)")

    return parser.parse_args()



def main() -> None:
    args = parse_args()
    gen_path = args.gen_path
    failed_files: list[str] = []
    ignored_errors = IgnoredErrorsList(args.ignored_errors_dir, regex_list=args.extra_ignored_regexes)
    # Strip trailing newlines from regex arguments
    args.tool_error_regex = args.tool_error_regex.rstrip('\n')
    if args.sim_error_regex:
        args.sim_error_regex = args.sim_error_regex.rstrip('\n')
    stats = Counter()
    all_found_errors: list[FoundError] = []

    for file_path in Path(gen_path).glob("*.sv"):
        file_path_str = str(file_path)
        with open(file_path, 'r', encoding='utf-8') as file:
            file_content = file.read()
        # Run analysis
        main_cmd = make_command(
            cmd=args.tool_cmd,
            file_path=file_path_str,
            file_content=file_content
        )
        cmd_res, found_errors = run_test(
            cmd=main_cmd,
            file_content=file_content,
            file_path=file_path_str,
            error_regex=args.tool_error_regex,
            ignored_errors=ignored_errors
        )
        for err in found_errors:
            all_found_errors.append(FoundError(err, file_path_str))
        if found_errors:
            failed_files.append(file_path_str)
            stats['failed'] += 1
        elif cmd_res:
            stats['clean'] += 1
        else:
            stats['handled_errors'] += 1
        # Run simulation if configured
        if cmd_res and args.sim_cmd:
            sim_res, sim_found_errors = run_test(
                cmd=args.sim_cmd,
                file_content=file_content,
                file_path=file_path_str,
                error_regex=args.sim_error_regex,
                ignored_errors=ignored_errors
            )
            for err in sim_found_errors:
                all_found_errors.append(FoundError(err, file_path_str))
            if sim_found_errors:
                failed_files.append(file_path_str)
                stats['failed'] += 1
            elif sim_res:
                stats['clean'] += 1
            else:
                stats['handled_errors'] += 1

    if all_found_errors:
        print_pretty([f"Error in {error.file_name}: {error.text}" for error in all_found_errors])

        # Compute NCD for all found errors
        ncd_results = compute_ncd_for_errors(all_found_errors, ".github/workflows/runner/ncd-xz.sh")
        plot_error_distances_mds(all_found_errors, ncd_results, output_path=args.error_distances_img)

    print_pretty([
        "Test Statistics:",
        f"Clean tests:    {stats['clean']}",
        f"Ignored errors: {stats['handled_errors']}",
        f"Failed tests:   {stats['failed']}",
        f"Total tests:    {sum(stats.values())}"
    ])
    
    if failed_files:
        print_pretty([
            f"  Total failed tests: {len(failed_files)}",
            "  Failed files:",
            *[f"  - {failed_file}" for failed_file in failed_files]
        ])
        sys.exit(1)
    else:
        print_pretty(["  All tests passed successfully."])


if __name__ == "__main__":
    main()
