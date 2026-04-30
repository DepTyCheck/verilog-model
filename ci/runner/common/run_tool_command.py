# ci/runner/common/run_tool_command.py
from dataclasses import dataclass, field

from common.command_config import CommandConfig
from common.command_output import CommandOutput
from common.error_types import KnownError
from common.handle_errors import ErrorMatcherProtocol
from common.per_file_report import MatchRecord
from common.run_command import ExecutionResult


@dataclass
class CommandResult:
    command: str
    outcome: str
    matches: list[MatchRecord] = field(default_factory=list)


def analyze_command(
    cmd: str,
    cmd_result: ExecutionResult,
    cmd_config: CommandConfig,
    ignored_errors: ErrorMatcherProtocol,
    file_path: str,
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

    analysis = CommandOutput(cmd_result.output).analyze(
        ignored_errors_list=ignored_errors,
        tool_error_regex=cmd_config.error_regex,
        file_path=file_path,
    )

    matches: list[MatchRecord] = []
    has_unknown = False
    seen_ignored_match = False

    for em in analysis.found_matches:
        err = em.match.error
        if isinstance(err, KnownError):
            matches.append(MatchRecord(error_id=err.error_id, matched_text=em.match.matched_text))
        else:
            # IgnoredError (extra-regex passthrough) — handled, but not stored in the per-file report.
            seen_ignored_match = True

    for unexpected in analysis.unexpected_errors:
        matches.append(MatchRecord(error_id="unknown", matched_text=unexpected.tool_output_error_text))
        has_unknown = True

    if not matches and not seen_ignored_match:
        # Tool failed and nothing was extracted by either tool regex or whole-output match —
        # record the whole output as a single unknown. (Per spec: full output, not an excerpt.)
        matches.append(MatchRecord(error_id="unknown", matched_text=cmd_result.output))
        has_unknown = True

    outcome = "unknown" if has_unknown else "known_errors"
    return CommandResult(command=cmd, outcome=outcome, matches=matches)
