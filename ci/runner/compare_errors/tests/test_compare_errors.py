import unittest
from dataclasses import dataclass

from compare_errors.compare_errors import ErrorPercentageDelta, ErrorsComparison


@dataclass
class _StubHistorical:
    pcts: dict[str, float]

    def error_ids(self) -> set[str]:
        return set(self.pcts)

    def historical_pct(self, error_id: str) -> float:
        return self.pcts[error_id]


@dataclass
class _StubCurrent:
    pcts: dict[str, float]

    def error_ids(self) -> set[str]:
        return set(self.pcts)

    def current_pct(self, error_id: str) -> float:
        return self.pcts[error_id]


class TestErrorPercentageDelta(unittest.TestCase):
    def test_positive_delta(self):
        d = ErrorPercentageDelta(error_id="foo", historical_pct=5.0, current_pct=12.5)
        self.assertAlmostEqual(d.delta_pct, 7.5)

    def test_zero_delta(self):
        d = ErrorPercentageDelta(error_id="foo", historical_pct=10.0, current_pct=10.0)
        self.assertAlmostEqual(d.delta_pct, 0.0)

    def test_negative_delta(self):
        d = ErrorPercentageDelta(error_id="foo", historical_pct=20.0, current_pct=5.0)
        self.assertAlmostEqual(d.delta_pct, -15.0)


class TestErrorsComparison(unittest.TestCase):
    def _comp(self, historical, current):
        return ErrorsComparison(_StubHistorical(historical), _StubCurrent(current))

    def test_includes_union_of_ids(self):
        comp = self._comp({"a": 10.0, "b": 5.0}, {"a": 7.0, "c": 3.0})
        ids = {d.error_id for d in comp.compare()}
        self.assertEqual(ids, {"a", "b", "c"})

    def test_historical_only_id_has_zero_current(self):
        comp = self._comp({"only_hist": 12.0}, {})
        d = next(x for x in comp.compare() if x.error_id == "only_hist")
        self.assertAlmostEqual(d.current_pct, 0.0)
        self.assertAlmostEqual(d.delta_pct, -12.0)

    def test_current_only_id_has_zero_historical(self):
        comp = self._comp({}, {"new_err": 8.0})
        d = next(x for x in comp.compare() if x.error_id == "new_err")
        self.assertAlmostEqual(d.historical_pct, 0.0)
        self.assertAlmostEqual(d.delta_pct, 8.0)

    def test_sorted_by_abs_delta_descending(self):
        comp = self._comp(
            {"small": 5.0, "huge_neg": 30.0, "huge_pos": 0.0},
            {"small": 6.0, "huge_neg": 5.0, "huge_pos": 32.0},
        )
        deltas = comp.compare()
        for i in range(len(deltas) - 1):
            self.assertGreaterEqual(abs(deltas[i].delta_pct), abs(deltas[i + 1].delta_pct))


if __name__ == "__main__":
    unittest.main()
