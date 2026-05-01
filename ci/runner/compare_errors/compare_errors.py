from dataclasses import dataclass
from typing import Protocol


class _Historical(Protocol):
    def error_ids(self) -> set[str]: ...
    def historical_pct(self, error_id: str) -> float: ...


class _Current(Protocol):
    def error_ids(self) -> set[str]: ...
    def current_pct(self, error_id: str) -> float: ...


@dataclass(frozen=True)
class ErrorPercentageDelta:
    error_id: str
    historical_pct: float
    current_pct: float

    @property
    def delta_pct(self) -> float:
        return self.current_pct - self.historical_pct


class ErrorsComparison:
    """Joins historical and current error percentages into a sorted delta list.

    `ReproducedIndex` is intentionally wired at the call site (`main.py`)
    rather than here so this class stays dependency-free and easily testable
    with stub indexes.
    """

    def __init__(self, historical: _Historical, current: _Current):
        self._historical = historical
        self._current = current

    def compare(self) -> list[ErrorPercentageDelta]:
        hist_ids = self._historical.error_ids()
        curr_ids = self._current.error_ids()
        all_ids = hist_ids | curr_ids
        deltas: list[ErrorPercentageDelta] = []
        for error_id in all_ids:
            h = self._historical.historical_pct(error_id) if error_id in hist_ids else 0.0
            c = self._current.current_pct(error_id) if error_id in curr_ids else 0.0
            deltas.append(ErrorPercentageDelta(error_id=error_id, historical_pct=h, current_pct=c))
        deltas.sort(key=lambda d: abs(d.delta_pct), reverse=True)
        return deltas
