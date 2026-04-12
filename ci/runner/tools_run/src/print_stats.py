from tools_run.src.tests_list import TestsRunResult
from tools_run.src.utils import print_pretty


def print_issues_count(result: TestsRunResult) -> None:
    stats = result.run_stats
    print_pretty(
        [
            "Test Statistics:",
            f"Clean tests:   {stats['clean']}",
            f"Known issues:  {stats['handled_errors']}",
            f"Failed tests:  {stats['failed']}",
            f"Total tests:   {sum(stats.values())}",
        ]
    )


def print_failed_tests_paths(result: TestsRunResult) -> None:
    unique_paths = list(dict.fromkeys(error.test_file_path for error in result.unexpected_errors))
    print_pretty(
        [
            f"  Total failed tests: {len(unique_paths)}",
            "  Failed tests:",
            *[f"  - {path}" for path in unique_paths],
        ]
    )


def print_unexpected_errors(result: TestsRunResult) -> None:
    """Print the raw error text for every unexpected error to stdout."""
    if not result.unexpected_errors:
        return
    lines = ["Unexpected error messages:"]
    for i, error in enumerate(result.unexpected_errors, 1):
        lines.append(f"  [{i}] {error.test_file_path}")
        lines.append(f"      {error.tool_output_error_text}")
    print_pretty(lines)
