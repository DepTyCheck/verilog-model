from common.markdown_table import build_markdown_table

from .compare_errors import ErrorPercentageDelta


def _pct_str(pct: float) -> str:
    return f"{pct:.2f}%"


def _delta_cell(delta: float) -> str:
    rounded = round(delta, 2)
    if rounded > 0:
        return f"🟩 +{rounded:.2f}%"
    if rounded < 0:
        return f"🟥 {rounded:.2f}%"
    return "0.00%"


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

    Columns: Error ID, Historical %, Delta cur-hist, Master %, Delta cur-master,
    Current %, and optionally Reproduced.

    Historical % and Master % span different windows by design: Historical %
    covers an error's full lifetime (incl. legacy runs), while Master % only
    counts the window since master last changed, so the two do not reconcile.

    Delta cells are colored with emoji squares (🟩 positive, 🟥 negative) because
    GitHub sanitizes inline CSS in $GITHUB_STEP_SUMMARY markdown. Rows are sorted
    by abs(delta_vs_master) descending.

    known_errors: optional dict mapping error_id -> reproduced (True/False).
    When provided, a "Reproduced" column is appended showing ✅ or ❌.
    """
    if not deltas:
        return "No errors found in current or historical data."

    deltas = sorted(deltas, key=lambda d: abs(d.delta_vs_master), reverse=True)

    headers = [
        "Error ID",
        "Historical %",
        "Delta cur-hist",
        "Master %",
        "Delta cur-master",
        "Current %",
    ]
    alignments = ["left", "right", "left", "right", "left", "right"]

    if known_errors is not None:
        headers.append("Reproduced")
        alignments.append("left")

    rows = []
    for d in deltas:
        row = [
            _error_id_cell(d.error_id, error_url_prefix),
            _pct_str(d.historical_pct),
            _delta_cell(d.delta_pct),
            _pct_str(d.master_pct),
            _delta_cell(d.delta_vs_master),
            _pct_str(d.current_pct),
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
