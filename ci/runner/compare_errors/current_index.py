from pathlib import Path

from common.per_file_report import load_report

_UNKNOWN_SENTINEL = "unknown"


class CurrentIndex:
    """Per-error_id occurrence percentages over the current PR's per-file JSONs.

    Loads every ``*.json`` under ``per_file_dir`` (the workflow pre-splits
    regression artifacts into a sibling directory, so the dir contains only
    run-tools outputs).

    Counts every ``files[].commands[].matches[]`` entry that has a real
    ``error_id`` (the literal ``"unknown"`` is a sentinel and is skipped).
    Each ``error_id`` is associated with exactly one tool (unique per tool by
    project convention). ``current_pct`` divides by the number of files that
    tool ran over.
    Each tool must contribute exactly one per-file JSON; a duplicate
    ``tool_name`` raises ``ValueError``.
    """

    def __init__(self, per_file_dir: str | Path):
        self._counts: dict[str, int] = {}
        self._owning_tool: dict[str, str] = {}
        self._tool_file_count: dict[str, int] = {}
        self._scan(Path(per_file_dir))

    def error_ids(self) -> set[str]:
        return set(self._counts)

    def current_pct(self, error_id: str) -> float:
        if error_id not in self._counts:
            raise KeyError(f"error_id {error_id!r} not in current index")
        tool = self._owning_tool[error_id]
        file_count = self._tool_file_count[tool]
        if file_count == 0:
            raise ValueError(f"tool {tool!r}: zero files, cannot compute pct for {error_id!r}")
        return self._counts[error_id] / file_count * 100.0

    def _scan(self, root: Path) -> None:
        if not root.exists():
            return
        for path in sorted(root.glob("*.json")):
            self._ingest(path)

    def _ingest(self, path: Path) -> None:
        report = load_report(path)
        tool = report.tool_name
        if tool in self._tool_file_count:
            raise ValueError(f"{path}: tool {tool!r} already ingested from another per-file JSON; " "expected one per-file JSON per tool.")
        self._tool_file_count[tool] = len(report.files)
        for f in report.files:
            for c in f.commands:
                for m in c.matches:
                    if m.error_id == _UNKNOWN_SENTINEL:
                        continue
                    self._counts[m.error_id] = self._counts.get(m.error_id, 0) + 1
                    self._owning_tool.setdefault(m.error_id, tool)
