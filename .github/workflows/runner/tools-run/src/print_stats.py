from src.utils import print_pretty
from src.tests_list import TestsRunResult


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
    print_pretty(
        [
            f"  Total failed tests: {len(result.unexpected_errors)}",
            "  Failed tests:",
            *[f"  - {error.test_file_path}" for error in result.unexpected_errors],
        ]
    )
