from .report_structure import RunInfo


def total_test_count(runs: list[RunInfo]) -> int:
    """Sum of test cases across all runs."""
    return sum(run.amount for run in runs)


def occurrence_pct(count: int, total: int) -> float:
    """Percentage of runs in which this error occurred (0.0–100.0)."""
    if total == 0:
        return 0.0
    return (count / total) * 100
