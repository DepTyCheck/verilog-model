import subprocess
from dataclasses import dataclass

from common.logger import get_logger

COMMAND_TIMEOUT_MINUTES = 7
COMMAND_TIMEOUT_SECONDS = COMMAND_TIMEOUT_MINUTES * 60


@dataclass
class ExecutionResult:
    command_executed_successfully: bool
    result_code_is_ok: bool
    timed_out: bool
    output: str


def run_command(cmd: str, cwd: str | None = None) -> ExecutionResult:
    """
    Execute a shell command and capture its combined stdout+stderr output.

    Returns an ExecutionResult with timed_out=True when the command exceeds
    COMMAND_TIMEOUT_MINUTES, leaving result_code_is_ok=False so callers can
    distinguish a timeout from a clean pass.
    """
    get_logger().info(f"Execute: {cmd}")

    try:
        result = subprocess.run(
            cmd,
            shell=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            timeout=COMMAND_TIMEOUT_SECONDS,
            check=False,
            cwd=cwd,
        )
        get_logger().debug(f"Exit code: {result.returncode}. Output:\n{result.stdout}")

        return ExecutionResult(
            command_executed_successfully=True,
            result_code_is_ok=result.returncode == 0,
            timed_out=False,
            output=result.stdout,
        )

    except subprocess.TimeoutExpired as timeout_error:
        msg = f"Command timed out after {COMMAND_TIMEOUT_MINUTES} minutes: {timeout_error}"
        get_logger().warning(msg)

        return ExecutionResult(
            command_executed_successfully=True,
            result_code_is_ok=False,
            timed_out=True,
            output=msg,
        )

    except Exception as error:
        get_logger().error(f"Command execution failed: {error}")

        return ExecutionResult(
            command_executed_successfully=False,
            result_code_is_ok=False,
            timed_out=False,
            output=str(error),
        )
