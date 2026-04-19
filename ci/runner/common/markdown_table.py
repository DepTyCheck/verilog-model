"""
Generic GitHub-Flavored Markdown table builder.

Usage:
    from common.markdown_table import build_markdown_table

    table = build_markdown_table(
        headers=["Name", "Value", "Score"],
        rows=[["foo", "bar", "42"]],
        alignments=["left", "left", "right"],
        title="Results",
    )

Column alignment in alignments:
    "left"  — default, left-aligned content and GFM :----- separator
    "right" — right-aligned content and GFM -----: separator
"""


def build_markdown_table(
    headers: list[str],
    rows: list[list[str]],
    alignments: list[str] | None = None,
    title: str = "",
) -> str:
    """
    Build a GitHub-Flavored Markdown table.

    headers:    column header labels
    rows:       list of rows; each row is a list of cell strings matching headers
    alignments: per-column "left" or "right" (defaults to "left" for all columns)
    title:      optional h2 heading printed before the table
    """
    n = len(headers)
    aligns = list(alignments or []) + ["left"] * n
    aligns = aligns[:n]

    col_widths = [len(h) for h in headers]
    for row in rows:
        for i, cell in enumerate(row[:n]):
            col_widths[i] = max(col_widths[i], len(cell))

    def _cell(text: str, width: int, align: str) -> str:
        return f"{text:>{width}}" if align == "right" else f"{text:<{width}}"

    def _row(cells: list[str]) -> str:
        return "| " + " | ".join(_cell(c, col_widths[i], aligns[i]) for i, c in enumerate(cells[:n])) + " |"

    def _sep_cell(i: int) -> str:
        dashes = "-" * col_widths[i]
        return f"-{dashes}:" if aligns[i] == "right" else f"-{dashes}-"

    sep = "|" + "".join(f"{_sep_cell(i)}|" for i in range(n))

    lines: list[str] = []
    if title:
        lines += [f"## {title}", ""]
    lines.append(_row(headers))
    lines.append(sep)
    for row in rows:
        lines.append(_row(row))
    return "\n".join(lines)
