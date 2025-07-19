import subprocess
from find_top import find_top
from handle_errors import handle_errors
from ignored_errors_list import IgnoredErrorsList

COMMAND_TIMEOUT_MINUTES = 7
COMMAND_TIMEOUT_SECONDS = COMMAND_TIMEOUT_MINUTES*60

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
