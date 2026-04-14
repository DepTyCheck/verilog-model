from common.command_output import AnalyzisResult
from common.tool_matrix_runner import FileInput
from tools_run.src.utils import print_pretty


def count_run_stats(results: list[tuple[FileInput, AnalyzisResult]]) -> dict:
    """Classify each result as clean / handled_errors / failed."""
    clean = handled = failed = 0
    for _, result in results:
        if result.unexpected_errors:
            failed += 1
        elif result.found_matches:
            handled += 1
        else:
            clean += 1
    return {"clean": clean, "handled_errors": handled, "failed": failed}


def print_issues_count(stats: dict) -> None:
    print_pretty(
        [
            "Test Statistics:",
            f"Clean tests:   {stats['clean']}",
            f"Known issues:  {stats['handled_errors']}",
            f"Failed tests:  {stats['failed']}",
            f"Total tests:   {sum(stats.values())}",
        ]
    )
