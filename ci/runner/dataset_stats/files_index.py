import re
from datetime import date, datetime
from pathlib import Path

_DATE_RE = re.compile(r"^(\d{4})_(\d{2})_(\d{2})-seed_\d+_\d+\.\w+$")


class FilesIndex:
    """Indexes files in <dataset>/files/ by their YYYY_MM_DD date prefix."""

    def __init__(self, files_dir: str | Path):
        self._dates: list[date] = []
        self._scan(Path(files_dir))

    def count_in_window(self, start: date, end: date) -> int:
        """Inclusive on both endpoints. Returns 0 if start > end."""
        if start > end:
            return 0
        return sum(1 for d in self._dates if start <= d <= end)

    def _scan(self, root: Path) -> None:
        for path in sorted(root.iterdir()):
            if not path.is_file():
                continue
            m = _DATE_RE.match(path.name)
            if not m:
                raise ValueError(f"{path}: filename does not match YYYY_MM_DD-seed_*_*.<ext> pattern")
            try:
                d = datetime.strptime(f"{m.group(1)}-{m.group(2)}-{m.group(3)}", "%Y-%m-%d").date()
            except ValueError as exc:
                raise ValueError(f"{path}: unparseable date prefix: {exc}") from exc
            self._dates.append(d)
