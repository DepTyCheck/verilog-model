import tempfile
import uuid
from pathlib import Path

from common.assets import Assets
from common.command_config import CommandConfig
from common.command_output import AnalyzisResult
from common.error_types import UnexpectedError
from common.ignored_errors_list import IgnoredErrorsList
from common.logger import get_logger
from common.make_command import make_command
from common.run_command import run_command
from common.run_tool_command import analyze_result


def run_file(
    content: str,
    commands: list[CommandConfig],
    known_errors: IgnoredErrorsList,
    file_suffix: str,
    assets: Assets | None = None,
    logical_name: str | None = None,
) -> AnalyzisResult:
    """
    Write content to a temp file inside a temp directory, run commands in
    sequence (stopping on first failure), and return the AnalyzisResult.

    logical_name: if provided, used as the file path in error records instead
                  of the actual temp path — preserves meaningful paths for callers
                  that already know the original file location.
    """
    with tempfile.TemporaryDirectory(dir=Path.cwd()) as tmp_dir:
        if assets is not None:
            assets.copy_to_tmp_dir(tmp_dir)

        file_name = f"{uuid.uuid4().hex}{file_suffix}"
        tmp_path = str(Path(tmp_dir) / file_name)
        Path(tmp_path).write_text(content, encoding="utf-8")

        report_path = logical_name if logical_name is not None else tmp_path

        for cmd_config in commands:
            try:
                cmd = make_command(cmd_config.run, tmp_path, content)
            except Exception as exc:
                get_logger().warning(f"make_command failed: {exc}")
                return AnalyzisResult(
                    found_matches=[],
                    unexpected_errors=[
                        UnexpectedError(
                            tool_output_error_text=str(exc),
                            test_file_path=report_path,
                        )
                    ],
                    all_errors_are_known=False,
                )

            cmd_result = run_command(cmd, cwd=tmp_dir)

            if cmd_result.timed_out:
                return AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)

            success, result = analyze_result(cmd_result, cmd_config, known_errors, report_path)
            if not success:
                return result

        return AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)
