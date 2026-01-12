import re
import subprocess
from dataclasses import dataclass

from src.command_output import CommandOutput

COMMAND_TIMEOUT_MINUTES = 7
COMMAND_TIMEOUT_SECONDS = COMMAND_TIMEOUT_MINUTES * 60


def find_top(file_content: str) -> str:
    """
    Find the top module name in the given file content.

    Args:
        file_content (str): The content of the file to search for the top module

    Returns:
        str: The name of the top module, or None if no top module is found
    """
    matches = re.findall(r"(?<=module )[A-z]+", file_content, re.MULTILINE)
    if matches:
        return matches[-1]

    raise Exception("No top module found")


@dataclass
class ExecutionResult:
    command_executed_successfully: bool
    result_code_is_OK: bool
    output: CommandOutput


class RunCommand:
    def __init__(self, raw_str_cmd: str, file_path: str, file_content: str):
        self.cmd = self.make_command(raw_str_cmd, file_path, file_content)

    def make_command(self, cmd: str, file_path: str, file_content: str) -> str:
        """
        Construct a command string to run a tool.

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

    def execute(self) -> ExecutionResult:
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
        print(f"Execute: {self.cmd}")

        try:
            result = subprocess.run(
                self.cmd,
                shell=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                timeout=COMMAND_TIMEOUT_SECONDS,
            )
            output = result.stdout
            print(f"Exit code: {result.returncode}. Output:\n{output}")

            return ExecutionResult(command_executed_successfully=True, result_code_is_OK=result.returncode == 0, output=CommandOutput(out=output))

        except subprocess.TimeoutExpired as timeout_error:
            print(f"""Command timed out after {COMMAND_TIMEOUT_MINUTES} minutes: {timeout_error}""")

            return ExecutionResult(
                command_executed_successfully=True,
                result_code_is_OK=False,
                output=CommandOutput(out=f"Command timed out after {COMMAND_TIMEOUT_MINUTES} minutes: {timeout_error}"),
            )

        except Exception as error:
            print(f"Command execution failed: {error}")

            return ExecutionResult(
                command_executed_successfully=False,
                result_code_is_OK=False,
                output=CommandOutput(out=str(error)),
            )
