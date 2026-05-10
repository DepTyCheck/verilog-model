"""Tests for ``common.first_found_index.FirstFoundIndex``.

The class is used by both ``dataset_stats`` and ``legacy_stats``, which keep
their own fixture trees (``found_issues/`` and ``found_errors/`` respectively)
under their package's ``tests/data/`` directory. Each test runs against every
registered fixture via ``subTest`` so any divergence in behaviour between
fixture shapes surfaces immediately.
"""

import os
import unittest
from datetime import date

from common.first_found_index import FirstFoundIndex

_RUNNER_DIR = os.path.normpath(os.path.join(os.path.dirname(__file__), "..", ".."))


def _fixture(package: str, prefix: str) -> tuple[str, str]:
    data_dir = os.path.join(_RUNNER_DIR, package, "tests", "data")
    return data_dir, prefix


_FIXTURES: list[tuple[str, str]] = [
    _fixture("dataset_stats", "found_issues"),
    _fixture("legacy_stats", "found_errors"),
]


class TestFirstFoundIndexHappyPath(unittest.TestCase):
    def _idx(self, data_dir: str, prefix: str) -> FirstFoundIndex:
        return FirstFoundIndex(os.path.join(data_dir, prefix))

    def _for_each(self, fn) -> None:
        for data_dir, prefix in _FIXTURES:
            with self.subTest(package=os.path.relpath(data_dir, _RUNNER_DIR), prefix=prefix):
                fn(self._idx(data_dir, prefix))

    def test_single_example(self):
        self._for_each(lambda idx: self.assertEqual(idx.lookup("alpha"), date(2025, 7, 19)))

    def test_multiple_examples_earliest_wins(self):
        self._for_each(lambda idx: self.assertEqual(idx.lookup("beta"), date(2025, 3, 4)))

    def test_separate_tool_dirs(self):
        self._for_each(lambda idx: self.assertEqual(idx.lookup("delta"), date(2026, 2, 15)))

    def test_missing_id_raises(self):
        def check(idx: FirstFoundIndex) -> None:
            with self.assertRaises(KeyError):
                idx.lookup("not_present")

        self._for_each(check)


class TestFirstFoundIndexErrors(unittest.TestCase):
    def _bad_path(self, data_dir: str, prefix: str, suffix: str) -> str:
        return os.path.join(data_dir, f"{prefix}_{suffix}")

    def _for_each_error(self, suffix: str, expected_msg: str) -> None:
        for data_dir, prefix in _FIXTURES:
            with self.subTest(package=os.path.relpath(data_dir, _RUNNER_DIR), prefix=prefix, suffix=suffix):
                with self.assertRaises(ValueError) as ctx:
                    FirstFoundIndex(self._bad_path(data_dir, prefix, suffix))
                self.assertIn(expected_msg, str(ctx.exception))

    def test_missing_first_found_raises(self):
        self._for_each_error("missing_first_found", "no parseable first_found")

    def test_unparsable_first_found_raises(self):
        self._for_each_error("unparsable", "unparsable first_found")

    def test_duplicate_id_across_yaml_files_raises(self):
        self._for_each_error("duplicate", "duplicate error_id")
