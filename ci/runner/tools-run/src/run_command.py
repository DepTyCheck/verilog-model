import re
import subprocess
from dataclasses import dataclass

from src.command_output import CommandOutput
from src.logger import get_logger

COMMAND_TIMEOUT_MINUTES = 7
COMMAND_TIMEOUT_SECONDS = COMMAND_TIMEOUT_MINUTES * 60


def find_top_module_sv(file_content: str) -> str:
    """
    Find the top module name in the given SystemVerilog design.

    Returns:
        str: The name of the top module
    """
    matches = re.findall(r"(?<=module )[A-z]+", file_content, re.MULTILINE)
    if matches:
        return matches[-1]

    raise Exception("No top module found")


def find_top_entity_vhdl(file_content: str) -> str:
    """
    Find the top entity name in the given VHDL design.

    Returns:
        str: The name of the top entity
    """
    matches = re.findall(r"(?<=entity )[A-z]+", file_content, re.MULTILINE)
    if matches:
        return matches[-1]

    raise Exception("No top entity found")


@dataclass
class ExecutionResult:
    command_executed_successfully: bool
    result_code_is_ok: bool
    output: CommandOutput


class RunCommand:
    def __init__(
        self,
        raw_str_cmd: str,
        file_path: str,
        file_content: str,
        cwd: str | None = None,
    ):
        self.cmd = self.make_command(raw_str_cmd, file_path, file_content)
        self.cwd = cwd

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
            command = command.replace("{top_module}", find_top_module_sv(file_content))
        if "{vhdl_top_entity}" in command:
            command = command.replace("{vhdl_top_entity}", find_top_entity_vhdl(file_content))
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
        get_logger().info(f"Execute: {self.cmd}")

        try:
            result = subprocess.run(
                self.cmd,
                shell=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                timeout=COMMAND_TIMEOUT_SECONDS,
                check=False,
                cwd=self.cwd,
            )
            output = result.stdout
            get_logger().debug(f"Exit code: {result.returncode}. Output:\n{output}")

            return ExecutionResult(
                command_executed_successfully=True,
                result_code_is_ok=result.returncode == 0,
                output=CommandOutput(out=output),
            )

        except subprocess.TimeoutExpired as timeout_error:
            get_logger().warning(f"""Command timed out after {COMMAND_TIMEOUT_MINUTES} minutes: {timeout_error}""")

            return ExecutionResult(
                command_executed_successfully=True,
                result_code_is_ok=False,
                output=CommandOutput(out=f"Command timed out after {COMMAND_TIMEOUT_MINUTES} minutes: {timeout_error}"),
            )

        except Exception as error:
            get_logger().error(f"Command execution failed: {error}")

            return ExecutionResult(
                command_executed_successfully=False,
                result_code_is_ok=False,
                output=CommandOutput(out=str(error)),
            )
