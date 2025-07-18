#!/usr/bin/env python3

import sys
import subprocess
import argparse
from pathlib import Path
from find_top import find_top
from handle_errors import handle_errors
from ignored_errors_list import IgnoredErrorsList
from collections import Counter
from found_error import FoundError, compute_ncd_for_errors, plot_error_distances_mds
from typing import List

COMMAND_TIMEOUT_MINUTES = 7
COMMAND_TIMEOUT_SECONDS = COMMAND_TIMEOUT_MINUTES*60

def print_pretty(content: List[str]) -> None:
    print("\n======================================================================================")
    for line in content:
        print(line)
    print("======================================================================================")

def parse_args():
    parser = argparse.ArgumentParser(
        description="Run analysis and simulation tests over generated modules."
    )

    parser.add_argument('--gen-path', type=str, help="Path to generated modules", required=True)
    parser.add_argument('--tool-cmd', type=str, help="Analysis tool command", required=True)
    parser.add_argument('--tool-error-regex', type=str, help="Regex for analysis errors", required=True)

    parser.add_argument('--sim-cmd', type=str, help="Simulator command", required=False)
    parser.add_argument('--sim-error-regex', type=str, help="Regex for simulation errors", required=False)

    parser.add_argument('--errors-file', type=str, help="Path to regex file with allowed errors", required=True)
    parser.add_argument('--error-distances-img', type=str, help="Path to save error distances image", required=True)

    return parser.parse_args()

def execute_command(cmd: str) -> tuple[str, int]:
    """
    Execute a shell command and capture its output.

    Args:
        cmd (str): The shell command to execute

    Returns:
        tuple[str, int]: A tuple containing:
            - The command's output (stdout + stderr combined)
            - The exit code (0 for success, non-zero for failure)

    Note:
        If the command execution fails due to an exception, returns the error message
        and exit code 1.
    """
    print(f"Execute: {cmd}")

    try:
        result = subprocess.run(
            cmd,
            shell=True,
            capture_output=True,
            text=True,
            timeout=COMMAND_TIMEOUT_SECONDS
        )
        output = result.stdout + result.stderr
        print(f"Exit code: {result.returncode}. Output:\n{output}")
        return output, result.returncode
    except subprocess.TimeoutExpired as timeout_error:
        print(f"Command timed out after {COMMAND_TIMEOUT_MINUTES} minutes: {timeout_error}")
        return f"Command timed out after {COMMAND_TIMEOUT_MINUTES} minutes: {timeout_error}", 1
    except Exception as error:
        print(f"Command execution failed: {error}")
        return str(error), 1

def make_command(
    cmd: str,
    file_path: str,
    file_content: str
) -> str:
    """
    Construct a command string for running analysis or simulation tools.

    Args:
        cmd (str): The complete command string (binary + options)
        file_path (str): Path to the file to process
        file_content (str): Content of the file (used to find top module)

    Returns:
        str: The complete command string ready for execution
    """
    command = cmd
    if "{top_module}" in command:
        command = command.replace("{top_module}", find_top(file_content))
    command = command.replace("{file}", file_path)
    return command

def print_file(file_content: str, file_path: str) -> None:
    """Print the contents of a file."""
    print(f"\nThe entire content of {file_path}:")
    print(file_content)
    print("")

def run_test(
    cmd: str,
    file_content: str,
    file_path: str,
    error_regex: str,
    ignored_errors: IgnoredErrorsList
) -> tuple[bool, list[str]]:
    """
    Run a single test (analysis or simulation) and handle its errors.
    Returns:
        - Whether the command executed successfully or failed
        - List of error texts that are not ignored
    """
    output, exit_code = execute_command(cmd)
    if exit_code != 0:
        found_errors = handle_errors(
            output,
            error_regex,
            ignored_errors,
        )
        if found_errors:
            print_file(file_content, file_path)
        return False, found_errors
    return True, []

def main() -> None:
    args = parse_args()
    gen_path = args.gen_path
    failed_files: list[str] = []
    ignored_errors = IgnoredErrorsList(args.errors_file)
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
