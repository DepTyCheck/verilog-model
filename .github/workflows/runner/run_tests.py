#!/usr/bin/env python3

import sys
import subprocess
import argparse
from pathlib import Path
from find_top import find_top
from handle_errors import handle_errors
from ignored_errors_list import IgnoredErrorsList

def parse_args():
    parser = argparse.ArgumentParser(
        description="Run analysis and simulation tests over generated modules."
    )

    parser.add_argument('--gen-path', type=str, help="Path to generated modules", required=True)
    parser.add_argument('--tool-cmd', type=str, help="Analysis tool command (e.g., 'iverilog -Wall')", required=True)
    parser.add_argument('--tool-error-regex', type=str, help="Regex for analysis errors", required=True)

    parser.add_argument('--sim-cmd', type=str, help="Simulator command (e.g., 'vvp -n')", required=False)
    parser.add_argument('--sim-error-regex', type=str, help="Regex for simulation errors", required=False)

    parser.add_argument('--errors-file', type=str, help="Path to regex file with allowed errors", required=True)

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
            text=True
        )
        output = result.stdout + result.stderr
        print(f"Exit code: {result.returncode}. Output:\n{output}")
        return output, result.returncode
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

def run_test(
    cmd: str,
    file_content: str,
    file_path: str,
    error_regex: str,
    ignored_errors: IgnoredErrorsList
) -> tuple[bool, bool]:
    """
    Run a single test (analysis or simulation) and handle its errors.

    Args:
        cmd (str): Complete command string
        file_content (str): Content of the file
        error_regex (str): Regex pattern for error matching
        ignored_errors (IgnoredErrorsList): List of ignored error patterns

    Returns:
        A tuple containing:
            - Whether the command executed successfully or failed
            - Whether any errors were handled successfully or not
    """
    output, exit_code = execute_command(cmd)

    if exit_code != 0:
        handle_res = handle_errors(
            output,
            error_regex,
            ignored_errors,
            file_content,
            file_path
        )
        return False, handle_res
    return True, True

def main() -> None:
    args = parse_args()

    gen_path = args.gen_path
    failed_files: list[str] = []
    ignored_errors = IgnoredErrorsList(args.errors_file)

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
        cmd_res, cmd_handle_res = run_test(
            cmd=main_cmd,
            file_content=file_content,
            file_path=file_path_str,
            error_regex=args.tool_error_regex,
            ignored_errors=ignored_errors
        )
        if not cmd_handle_res:
            failed_files.append(file_path_str)

        # Run simulation if configured
        if cmd_res and args.sim_cmd:
            sim_res, sim_handle_res = run_test(
                cmd=args.sim_cmd,
                file_content=file_content,
                file_path=file_path_str,
                error_regex=args.sim_error_regex,
                ignored_errors=ignored_errors
            )
            if not sim_handle_res:
                failed_files.append(file_path_str)

    if failed_files:
        print("\n===========================================")
        print(f"  Total failed tests: {len(failed_files)}")
        print("  Failed files:")
        for failed_file in failed_files:
            print(f"  - {failed_file}")
        print("===========================================")
        sys.exit(1)
    else:
        print("\n===========================================")
        print("  All tests passed successfully.")
        print("===========================================")


if __name__ == "__main__":
    main()
