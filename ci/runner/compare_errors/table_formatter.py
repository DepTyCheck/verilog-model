from common.markdown_table import build_markdown_table

from .compare_errors import ErrorPercentageDelta


def _pct_str(pct: float) -> str:
    return f"{pct:.2f}%"


def _delta_str(delta: float) -> str:
    if delta > 0:
        return f"+{delta:.2f}%"
    return f"{delta:.2f}%"


def _error_id_cell(error_id: str, error_url_prefix: str | None) -> str:
    if error_url_prefix is None:
        return error_id
    return f"[{error_id}]({error_url_prefix}/{error_id})"


def format_table(
    deltas: list[ErrorPercentageDelta],
    error_url_prefix: str | None = None,
    known_errors: dict[str, bool] | None = None,
) -> str:
    """
    Format a Markdown comparison table.

    known_errors: optional dict mapping error_id -> reproduced (True/False).
    When provided, a "Reproduced" column is appended showing ✅ or ❌.
    """
    if not deltas:
        return "No errors found in current or historical data."

    deltas = sorted(deltas, key=lambda d: abs(d.delta_pct), reverse=True)

    headers = ["Error ID", "Historical %", "Current %", "Delta %"]
    alignments = ["left", "right", "right", "right"]

    if known_errors is not None:
        headers.append("Reproduced")
        alignments.append("left")

    rows = []
    for d in deltas:
        row = [
            _error_id_cell(d.error_id, error_url_prefix),
            _pct_str(d.historical_pct),
            _pct_str(d.current_pct),
            _delta_str(d.delta_pct),
        ]
        if known_errors is not None:
            row.append("✅" if known_errors.get(d.error_id, False) else "❌")
        rows.append(row)

    return build_markdown_table(
        headers=headers,
        rows=rows,
        alignments=alignments,
        title="Error Statistics Comparison",
    )
