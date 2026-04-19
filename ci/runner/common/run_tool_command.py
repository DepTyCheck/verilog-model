from common.command_config import CommandConfig
from common.command_output import AnalyzisResult, CommandOutput
from common.error_types import UnexpectedError
from common.handle_errors import ErrorMatcherProtocol
from common.run_command import ExecutionResult


def analyze_result(
    cmd_result: ExecutionResult,
    cmd_config: CommandConfig,
    ignored_errors: ErrorMatcherProtocol,
    file_path: str,
) -> tuple[bool, AnalyzisResult]:
    """
    Analyze a command execution result against known error patterns.

    Returns:
        (True,  clean_result)    — command exited with code 0
        (False, analysis_result) — command failed; output analyzed via CommandOutput.analyze
                                   or returned as raw excerpt when no error_regex is configured
    """
    if cmd_result.result_code_is_ok:
        return True, AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)

    if cmd_config.error_regex is None:
        excerpt = "\n".join(cmd_result.output.splitlines()[:3])
        return False, AnalyzisResult(
            found_matches=[],
            unexpected_errors=[UnexpectedError(tool_output_error_text=excerpt, test_file_path=file_path)],
            all_errors_are_known=False,
        )

    analysis = CommandOutput(cmd_result.output).analyze(
        ignored_errors_list=ignored_errors,
        tool_error_regex=cmd_config.error_regex,
        file_path=file_path,
    )
    return False, analysis
