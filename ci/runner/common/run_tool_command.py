# ci/runner/common/run_tool_command.py
from dataclasses import dataclass, field

from common.command_config import CommandConfig
from common.ignored_errors_list import IgnoredErrorsList
from common.per_file_report import MatchRecord
from common.run_command import ExecutionResult
from cwm import parse_messages


@dataclass
class CommandResult:
    command: str
    outcome: str
    matches: list[MatchRecord] = field(default_factory=list)


def analyze_command(
    cmd: str,
    cmd_result: ExecutionResult,
    cmd_config: CommandConfig,
    ignored_errors: IgnoredErrorsList,
    location_regex: str | None = None,
) -> CommandResult:
    """
    Classify one command execution into a CommandResult.

    Outcome rules:
      - timeout       — command timed out
      - clean         — exit code 0
      - known_errors  — non-zero exit, regex matched only registered KnownErrors
      - unknown       — non-zero exit + at least one match without a registered KnownError,
                        OR no regex configured, OR regex matched nothing
    """
    if cmd_result.timed_out:
        return CommandResult(command=cmd, outcome="timeout", matches=[])

    if cmd_result.result_code_is_ok:
        return CommandResult(command=cmd, outcome="clean", matches=[])

    if cmd_config.error_regex is None:
        # Per spec: with no regex configured, every non-zero exit is unknown,
        # and the whole tool output goes into matched_text (no excerpt).
        return CommandResult(
            command=cmd,
            outcome="unknown",
            matches=[MatchRecord(error_id="unknown", matched_text=cmd_result.output)],
        )

    result = parse_messages(
        cmd_result.output,
        cmd_config.error_regex.regex,
        ignored_errors.needles,
        location_regex=location_regex,
        ignored_regexes=ignored_errors.ignored_regexes,
    )
    matches = [MatchRecord(error_id=m.needle_id, matched_text=m.matched_text) for m in result.matches]
    matches.extend(MatchRecord(error_id="unknown", matched_text=text) for text in result.unmatched_messages)
    outcome = "unknown" if result.unmatched_messages else "known_errors"
    return CommandResult(command=cmd, outcome=outcome, matches=matches)
