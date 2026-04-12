"""
Core regression-checking logic.

For each known error file, every example (minified and full) is run through
the configured tool.  Results are classified as:

  CLEAN       – tool passed with no errors (bug may be fixed)
  KNOWN_ERROR – tool failed and output matches the known error regex
  NEW_ERROR   – tool failed with a different / unexpected error
  TIMEOUT     – tool exceeded the execution time limit
  EXEC_ERROR  – tool could not be launched (command build or subprocess failure)

check_all scans ALL tool subdirectories under known_errors_dir (cross-tool
regression).  NEW_ERROR from any tool on any example causes CI failure.
For errors originally attributed to the checked tool, KNOWN_ERROR/CLEAN
results are also collected as regression_confirmations for a separate report.

Data flow:
  found_errors/{originating_tool}/*.yaml  (all subdirs scanned)
        │
        ▼ _load_all_error_files()
   ErrorFile(id, tool, regex, mode, examples)
        │
        ▼ for each error_file × for each example:
   make_command() → run_command() → classify()
        │
        ▼
   ExampleToolResult(status, output_excerpt)
"""

import re
import tempfile
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path

from common.error_file_parser import ErrorFile, Example, parse_error_files
from common.error_types import MatchingMode
from common.logger import get_logger
from common.make_command import make_command
from common.run_command import run_command
from common.tool_error_regex import ToolErrorRegex


class ExampleStatus(str, Enum):
    CLEAN = "clean"
    KNOWN_ERROR = "known_error"
    NEW_ERROR = "new_error"
    TIMEOUT = "timeout"
    EXEC_ERROR = "exec_error"


@dataclass
class ToolConfig:
    name: str
    cmd: str
    error_regex: ToolErrorRegex | None
    language: str = "sv"


@dataclass
class ExampleToolResult:
    tool_name: str
    status: ExampleStatus
    output_excerpt: str = ""

    def to_dict(self) -> dict:
        d: dict = {"status": self.status.value}
        if self.output_excerpt:
            d["output_excerpt"] = self.output_excerpt
        return d


@dataclass
class ExampleResult:
    example_name: str
    example_type: str  # "minified" | "full"
    results_by_tool: dict[str, ExampleToolResult] = field(default_factory=dict)

    def to_dict(self) -> dict:
        return {
            "name": self.example_name,
            "type": self.example_type,
            "results_by_tool": {tool: r.to_dict() for tool, r in self.results_by_tool.items()},
        }

    def new_errors(self) -> list[tuple[str, str]]:
        """Return list of (tool_name, output_excerpt) for all NEW_ERROR results."""
        return [(tool, r.output_excerpt) for tool, r in self.results_by_tool.items() if r.status == ExampleStatus.NEW_ERROR]


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


def _classify(
    output: str,
    result_code_ok: bool,
    timed_out: bool,
    error_file: ErrorFile,
    tool_regex: ToolErrorRegex | None,
) -> tuple[ExampleStatus, str]:
    """
    Classify a single tool run against one known error.

    Returns (status, output_excerpt).

    Decision tree:
      timed_out                              → TIMEOUT
      result_code_ok AND no extracted errors → CLEAN
      SPECIFIC mode (tool_regex present): extract errors with tool_regex
        all match error_file.regex           → KNOWN_ERROR
        any don't match                      → NEW_ERROR  (report non-matching)
      WHOLE mode (or no specific matches, or tool_regex is None):
        whole output matches error_file.regex → KNOWN_ERROR
        otherwise and result_code_ok          → CLEAN
        otherwise                             → NEW_ERROR
    """
    if timed_out:
        return ExampleStatus.TIMEOUT, output[:200]

    if tool_regex is not None and error_file.mode == MatchingMode.SPECIFIC:
        extracted = [m.group(0) for m in re.finditer(tool_regex.regex, output, re.MULTILINE)]
        if extracted:
            non_matching = [e for e in extracted if not re.search(error_file.regex, e, re.MULTILINE)]
            if non_matching:
                return ExampleStatus.NEW_ERROR, "\n".join(non_matching[:3])
            return ExampleStatus.KNOWN_ERROR, extracted[0]

    # Whole-output match (also fallback when SPECIFIC found nothing)
    whole_match = re.search(error_file.regex, output, re.MULTILINE)
    if whole_match:
        return ExampleStatus.KNOWN_ERROR, whole_match.group(0)

    if result_code_ok:
        return ExampleStatus.CLEAN, ""

    # Non-zero exit but nothing matched at all
    excerpt = "\n".join(output.splitlines()[:5])
    return ExampleStatus.NEW_ERROR, excerpt


def _run_example_with_tool(
    example: Example,
    error_file: ErrorFile,
    tool: ToolConfig,
) -> ExampleToolResult:
    """Write example content to a temp file, run the tool, and classify the result."""
    suffix = ".vhdl" if tool.language == "vhdl" else ".sv"
    tmp_path = None
    try:
        with tempfile.NamedTemporaryFile(mode="w", suffix=suffix, delete=False) as tmp:
            tmp.write(example.content)
            tmp_path = tmp.name

        try:
            cmd = make_command(tool.cmd, tmp_path, example.content)
        except Exception as e:
            get_logger().warning(f"make_command failed for {error_file.error_id}/{example.name}: {e}")
            return ExampleToolResult(tool_name=tool.name, status=ExampleStatus.EXEC_ERROR, output_excerpt=str(e))

        result = run_command(cmd)

        if not result.command_executed_successfully:
            return ExampleToolResult(tool_name=tool.name, status=ExampleStatus.EXEC_ERROR, output_excerpt=result.output[:200])

        status, excerpt = _classify(
            output=result.output,
            result_code_ok=result.result_code_is_ok,
            timed_out=result.timed_out,
            error_file=error_file,
            tool_regex=tool.error_regex,
        )
        return ExampleToolResult(tool_name=tool.name, status=status, output_excerpt=excerpt)

    finally:
        if tmp_path:
            Path(tmp_path).unlink(missing_ok=True)


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


def check_all(
    known_errors_dir: str,
    tool: ToolConfig,
) -> tuple[list[ErrorResult], list[dict], list[dict]]:
    """
    Run regression checks for a single tool against ALL known errors (all tools).

    Returns:
        (error_results, new_error_incidents, regression_confirmations)

    new_error_incidents: flat list of NEW_ERROR findings — any occurrence causes CI failure.
    regression_confirmations: flat list of results for errors originally attributed to
        this tool, used to report whether own known errors still reproduce.
    """
    new_error_incidents: list[dict] = []
    regression_confirmations: list[dict] = []
    error_results: list[ErrorResult] = []

    error_files = _load_all_error_files(known_errors_dir)
    get_logger().info(f"Checking {len(error_files)} known errors across all tools with '{tool.name}'")

    for error_file in error_files:
        if not error_file.examples:
            get_logger().warning(f"No examples in {error_file.error_id}, skipping")
            continue

        error_result = ErrorResult(
            error_id=error_file.error_id,
            originating_tool=error_file.tool,
        )

        for example in error_file.examples:
            example_result = ExampleResult(
                example_name=example.name,
                example_type=example.type,
            )

            result = _run_example_with_tool(example, error_file, tool)
            example_result.results_by_tool[tool.name] = result

            incident = {
                "error_id": error_file.error_id,
                "originating_tool": error_file.tool,
                "checked_with_tool": tool.name,
                "example_name": example.name,
                "example_type": example.type,
                "output_excerpt": result.output_excerpt,
            }

            if result.status == ExampleStatus.NEW_ERROR:
                new_error_incidents.append(incident)

            if error_file.tool == tool.name:
                regression_confirmations.append({**incident, "status": result.status.value})

            error_result.examples.append(example_result)

        error_results.append(error_result)

    return error_results, new_error_incidents, regression_confirmations
