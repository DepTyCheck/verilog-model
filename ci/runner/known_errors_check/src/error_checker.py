"""
Core regression-checking logic.

For each known error file, every example (minified and full) is run through
the configured tool using the same command execution and output analysis code
as tools_run.  Two independent results are tracked per example:

  unknown_errors  – output contained errors that matched no known pattern.
                    Any occurrence across ALL tool error files causes CI failure.

  reproduced      – (own-tool errors only) the specific error_id pattern was
                    found in the output.  Reported in the summary but does NOT
                    affect CI pass/fail.

Data flow:
  found_errors/{tool}/*.yaml  (all subdirs scanned)
        │
        ▼ _load_all_error_files()
   ErrorFile(id, tool, regex, mode, examples)
        │
        ▼ IgnoredErrorsList.from_error_files(all_files)
   all_known_errors   (cross-tool, used for unknown-error detection)
        │
        ▼ for each error_file × for each example:
   make_command() → run_command() → CommandOutput.analyze()
        │
        ├─ unexpected_errors  → new_error_incidents   (CI fail)
        └─ found_matches      → reproduced check      (own-tool only, info only)
"""

import tempfile
from dataclasses import dataclass, field
from pathlib import Path

from common.command_config import CommandConfig
from common.command_output import AnalyzisResult, CommandOutput
from common.error_file_parser import ErrorFile, parse_error_files
from common.error_types import IgnoredError, KnownError, UnexpectedError
from common.logger import get_logger
from common.make_command import make_command
from common.run_command import run_command
from tools_run.src.ignored_errors_list import IgnoredErrorsList


@dataclass
class ToolConfig:
    name: str
    commands: list[CommandConfig]
    language: str = "sv"


@dataclass
class ExampleResult:
    example_name: str
    example_type: str  # "minified" | "full"
    reproduced: bool

    def to_dict(self) -> dict:
        return {
            "name": self.example_name,
            "type": self.example_type,
            "reproduced": self.reproduced,
        }


@dataclass
class ErrorResult:
    error_id: str
    originating_tool: str
    examples: list[ExampleResult] = field(default_factory=list)

    def to_dict(self) -> dict:
        return {
            "error_id": self.error_id,
            "originating_tool": self.originating_tool,
            "examples": [e.to_dict() for e in self.examples],
        }


def _load_all_error_files(known_errors_dir: str) -> list[ErrorFile]:
    """Load all error files from every tool subdirectory under known_errors_dir."""
    base_dir = Path(known_errors_dir)
    if not base_dir.exists():
        get_logger().warning(f"Known errors directory does not exist: {base_dir.absolute()}")
        return []
    all_files: list[ErrorFile] = []
    for subdir in sorted(base_dir.iterdir()):
        if subdir.is_dir():
            all_files.extend(parse_error_files(str(subdir)))
    return all_files


def _run_example(
    example,
    commands: list[CommandConfig],
    all_known_errors: IgnoredErrorsList,
    language: str,
) -> AnalyzisResult:
    """
    Write example content to a temp file, run all commands (stopping on first
    failure), and return the analysis of the failing command's output.
    """
    suffix = ".vhdl" if language == "vhdl" else ".sv"
    tmp_path = None
    try:
        with tempfile.NamedTemporaryFile(mode="w", suffix=suffix, delete=False) as tmp:
            tmp.write(example.content)
            tmp_path = tmp.name

        for cmd_config in commands:
            try:
                cmd = make_command(cmd_config.run, tmp_path, example.content)
            except Exception as e:
                get_logger().warning(f"make_command failed for {example.name}: {e}")
                return AnalyzisResult(
                    found_matches=[],
                    unexpected_errors=[UnexpectedError(tool_output_error_text=str(e), test_file_path=tmp_path)],
                    all_errors_are_known=False,
                )

            result = run_command(cmd)

            if not result.command_executed_successfully:
                excerpt = result.output[:200]
                return AnalyzisResult(
                    found_matches=[],
                    unexpected_errors=[UnexpectedError(tool_output_error_text=excerpt, test_file_path=tmp_path)],
                    all_errors_are_known=False,
                )

            if result.result_code_is_ok:
                continue  # This command passed; try the next one

            if result.timed_out:
                # Timeout is not classified as an unknown error
                return AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)

            if cmd_config.error_regex is None:
                excerpt = "\n".join(result.output.splitlines()[:3])
                return AnalyzisResult(
                    found_matches=[],
                    unexpected_errors=[UnexpectedError(tool_output_error_text=excerpt, test_file_path=tmp_path)],
                    all_errors_are_known=False,
                )

            return CommandOutput(result.output).analyze(
                ignored_errors_list=all_known_errors,
                tool_error_regex=cmd_config.error_regex,
                file_path=tmp_path,
            )

        # All commands passed
        return AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)

    finally:
        if tmp_path:
            Path(tmp_path).unlink(missing_ok=True)


def check_all(
    known_errors_dir: str,
    tool: ToolConfig,
) -> tuple[list[ErrorResult], list[dict], list[dict]]:
    """
    Run regression checks for a single tool against ALL known errors (all tools).

    Returns:
        (error_results, new_error_incidents, regression_confirmations)

    error_results:            per-error reproducibility data for own-tool errors only.
    new_error_incidents:      flat list of unknown-error findings — any occurrence causes CI failure.
    regression_confirmations: flat list of reproduced/not-reproduced results for own-tool errors.
    """
    error_files = _load_all_error_files(known_errors_dir)
    all_known_errors = IgnoredErrorsList.from_error_files(error_files)
    get_logger().info(f"Checking {len(error_files)} known errors across all tools with '{tool.name}'")

    new_error_incidents: list[dict] = []
    regression_confirmations: list[dict] = []
    error_results: list[ErrorResult] = []

    for error_file in error_files:
        if not error_file.examples:
            get_logger().warning(f"No examples in {error_file.error_id}, skipping")
            continue

        is_own_error = error_file.tool == tool.name
        error_result = ErrorResult(error_id=error_file.error_id, originating_tool=error_file.tool)

        for example in error_file.examples:
            analysis = _run_example(example, tool.commands, all_known_errors, tool.language)

            # Unknown errors cause CI failure regardless of which tool owns the error file
            for unexpected in analysis.unexpected_errors:
                new_error_incidents.append(
                    {
                        "error_id": error_file.error_id,
                        "originating_tool": error_file.tool,
                        "example_name": example.name,
                        "example_type": example.type,
                        "output_excerpt": unexpected.tool_output_error_text,
                    }
                )

            # Reproducibility is only tracked for the current tool's own errors
            if is_own_error:
                reproduced = any(
                    isinstance(m.match.error, KnownError) and m.match.error.error_id == error_file.error_id for m in analysis.found_matches
                )
                regression_confirmations.append(
                    {
                        "error_id": error_file.error_id,
                        "example_name": example.name,
                        "example_type": example.type,
                        "reproduced": reproduced,
                    }
                )
                error_result.examples.append(
                    ExampleResult(
                        example_name=example.name,
                        example_type=example.type,
                        reproduced=reproduced,
                    )
                )

        if is_own_error:
            error_results.append(error_result)

    return error_results, new_error_incidents, regression_confirmations
