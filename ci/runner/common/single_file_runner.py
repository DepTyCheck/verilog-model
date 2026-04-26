# ci/runner/common/single_file_runner.py
import tempfile
import uuid
from pathlib import Path

from common.assets import Assets
from common.command_config import CommandConfig
from common.ignored_errors_list import IgnoredErrorsList
from common.logger import get_logger
from common.make_command import make_command
from common.run_command import run_command
from common.run_tool_command import CommandResult, MatchRecord, analyze_command


def run_file(
    content: str,
    commands: list[CommandConfig],
    known_errors: IgnoredErrorsList,
    file_suffix: str,
    assets: Assets | None = None,
    logical_name: str | None = None,
) -> list[CommandResult]:
    """
    Write content to a temp file, run commands in sequence (stop on first failing
    command), and return one CommandResult per command actually executed.

    Stops on first command whose outcome is not "clean" — matching legacy
    behaviour. Length of returned list is 1..N, where N == len(commands).
    """
    get_logger().debug(f"Processing: {logical_name or '(unnamed)'}")
    get_logger().debug(f"File content:\n{content}")

    results: list[CommandResult] = []

    with tempfile.TemporaryDirectory(dir=Path.cwd()) as tmp_dir:
        if assets is not None:
            assets.copy_to_tmp_dir(tmp_dir)

        tmp_path = str(Path(tmp_dir) / f"{uuid.uuid4().hex}{file_suffix}")
        Path(tmp_path).write_text(content, encoding="utf-8")

        report_path = logical_name if logical_name is not None else tmp_path

        for cmd_config in commands:
            try:
                cmd = make_command(cmd_config.run, tmp_path, content)
            except Exception as exc:
                get_logger().warning(f"make_command failed for {report_path!r}: {exc}")
                results.append(
                    CommandResult(
                        command=cmd_config.run,
                        outcome="unknown",
                        matches=[MatchRecord(error_id="unknown", matched_text=str(exc))],
                    )
                )
                return results

            cmd_result = run_command(cmd, cwd=tmp_dir)
            command_result = analyze_command(cmd, cmd_result, cmd_config, known_errors, report_path)
            results.append(command_result)
            if command_result.outcome != "clean":
                return results

        return results
