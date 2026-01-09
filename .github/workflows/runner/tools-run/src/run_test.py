import subprocess
from src.find_top import find_top
from src.handle_errors import (
    FoundMatch,
    UnexpectedErrorText,
    extract_and_classify_errors,
    match_whole_output,
)
from src.ignored_errors_list import IgnoredErrorsList
from src.error_match_in_test import ErrorMatchInTest

COMMAND_TIMEOUT_MINUTES = 7
COMMAND_TIMEOUT_SECONDS = COMMAND_TIMEOUT_MINUTES * 60


def execute_command(cmd: str) -> tuple[str, int]:
    """
    Execute a shell command and capture its output.

    Args:
        cmd (str): The shell command to execute

    Returns:
        tuple[str, int]: A tuple containing:
            - The command's output
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
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            timeout=COMMAND_TIMEOUT_SECONDS,
        )
        output = result.stdout
        print(f"Exit code: {result.returncode}. Output:\n{output}")
        return output, result.returncode
    except subprocess.TimeoutExpired as timeout_error:
        print(
            f"""Command timed out after {
                COMMAND_TIMEOUT_MINUTES} minutes: {timeout_error}"""
        )
        return (
            f"Command timed out after {COMMAND_TIMEOUT_MINUTES} minutes: {timeout_error}",
            0,
        )
    except Exception as error:
        print(f"Command execution failed: {error}")
        return str(error), 1


def make_command(cmd: str, file_path: str, file_content: str) -> str:
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
    ignored_errors: IgnoredErrorsList,
) -> tuple[bool, list[str], list[ErrorMatchInTest]]:
    """
    Run a single test (analysis or simulation) and handle its errors.
    Returns:
        - Whether the command executed successfully or failed
        - List of error texts that are not ignored, or matched ignored error regex if whole output matches
    """
    output, exit_code = execute_command(cmd)
    if exit_code != 0:
        # Match errors
        unexpected_errors, found_matches = extract_and_classify_errors(
            output,
            error_regex,
            ignored_errors,
            test_path=file_path,
        )

        # Match whole output
        if len(found_matches) == 0:
            print("Matching whole output")
            unexpected_error_whole = match_whole_output(output, ignored_errors)
            if unexpected_error_whole == None:
                unexpected_errors.append("\n".join(output.splitlines()[:3]))
            elif isinstance(unexpected_error_whole, FoundMatch):
                found_matches.append(
                    ErrorMatchInTest(match=unexpected_error_whole, test_path=file_path)
                )

        if len(unexpected_errors) > 0:
            print_file(file_content, file_path)
        return False, unexpected_errors, found_matches
    return True, [], []
