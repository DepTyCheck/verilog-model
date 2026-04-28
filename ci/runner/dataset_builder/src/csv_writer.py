import csv
from pathlib import Path

HEADER = (
    "when_issue_occurred",
    "tool_name",
    "tool_commit",
    "error_id",
    "model_commit",
    "filename",
)


def append_rows(csv_path: Path, rows: list[dict]) -> None:
    if not csv_path.exists():
        raise FileNotFoundError(f"{csv_path} does not exist. Create it on the dataset branch first.")
    with open(csv_path, "a", newline="", encoding="utf-8") as fh:
        writer = csv.writer(fh, quoting=csv.QUOTE_NONNUMERIC)
        for row in rows:
            writer.writerow([row[col] for col in HEADER])
