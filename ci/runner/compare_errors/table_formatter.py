from .compare_errors import ErrorPercentageDelta


def _pct_str(pct: float) -> str:
    return f"{pct:.2f}%"


def _delta_str(delta: float) -> str:
    if delta > 0:
        return f"+{delta:.2f}%"
    return f"{delta:.2f}%"


def format_table(deltas: list[ErrorPercentageDelta]) -> str:
    if not deltas:
        return "No errors found in current or historical data."

    deltas = sorted(deltas, key=lambda d: abs(d.delta_pct), reverse=True)

    col_id = "Error ID"
    col_hist = "Historical %"
    col_curr = "Current %"
    col_delta = "Delta %"

    id_width = max(len(col_id), max(len(d.error_id) for d in deltas))
    hist_width = max(
        len(col_hist), max(len(_pct_str(d.historical_pct)) for d in deltas)
    )
    curr_width = max(len(col_curr), max(len(_pct_str(d.current_pct)) for d in deltas))
    delta_width = max(len(col_delta), max(len(_delta_str(d.delta_pct)) for d in deltas))

    def row(error_id: str, hist: str, curr: str, delta: str) -> str:
        return (
            f"| {error_id:<{id_width}} "
            f"| {hist:>{hist_width}} "
            f"| {curr:>{curr_width}} "
            f"| {delta:>{delta_width}} |"
        )

    separator = (
        f"|-{'-' * id_width}-"
        f"|-{'-' * hist_width}-"
        f"|-{'-' * curr_width}-"
        f"|-{'-' * delta_width}-|"
    )

    lines = [
        "## Error Statistics Comparison",
        "",
        row(col_id, col_hist, col_curr, col_delta),
        separator,
    ]

    for d in deltas:
        lines.append(
            row(
                d.error_id,
                _pct_str(d.historical_pct),
                _pct_str(d.current_pct),
                _delta_str(d.delta_pct),
            )
        )

    return "\n".join(lines)
