from datetime import date, datetime
from pathlib import Path

import yaml


class FirstFoundIndex:
    def __init__(self, found_errors_dir: str | Path):
        self._index: dict[str, date] = {}
        self._scan(Path(found_errors_dir))

    def lookup(self, error_id: str) -> date:
        if error_id not in self._index:
            raise KeyError(f"No yaml found for error_id={error_id!r}")
        return self._index[error_id]

    def _scan(self, root: Path) -> None:
        for path in sorted(root.rglob("*.yaml")):
            if not path.is_file():
                continue
            self._ingest(path)

    def _ingest(self, path: Path) -> None:
        with open(path, "r", encoding="utf-8") as f:
            doc = yaml.safe_load(f)
        if not isinstance(doc, dict):
            raise ValueError(f"{path}: top-level yaml is not a mapping")
        error_id = doc.get("id")
        if not isinstance(error_id, str) or not error_id:
            raise ValueError(f"{path}: missing or non-string 'id'")

        earliest = self._earliest_first_found(doc.get("examples") or [], path)

        if error_id in self._index:
            raise ValueError(f"{path}: duplicate error_id {error_id!r} (already seen for another yaml)")
        self._index[error_id] = earliest

    @staticmethod
    def _earliest_first_found(examples: list, path: Path) -> date:
        dates: list[date] = []
        for example in examples:
            if not isinstance(example, dict):
                continue
            for _, body in example.items():
                if not isinstance(body, dict):
                    continue
                raw = body.get("first_found")
                if raw is None:
                    continue
                dates.append(FirstFoundIndex._parse_ddmmyyyy(raw, path))
        if not dates:
            raise ValueError(f"{path}: no parseable first_found in any example")
        return min(dates)

    @staticmethod
    def _parse_ddmmyyyy(raw: object, path: Path) -> date:
        if not isinstance(raw, str):
            raise ValueError(f"{path}: first_found must be a string, got {type(raw).__name__}")
        try:
            return datetime.strptime(raw, "%d.%m.%Y").date()
        except ValueError as exc:
            raise ValueError(f"{path}: unparseable first_found {raw!r}: {exc}") from exc
