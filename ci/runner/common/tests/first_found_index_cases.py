"""Reusable FirstFoundIndex test cases.

Per-package test modules call :func:`make_first_found_index_test_classes`
with their ``data_dir`` and YAML-tree ``prefix`` ("found_issues" /
"found_errors") to obtain a concrete ``(HappyPath, Errors)`` pair of
``unittest.TestCase`` subclasses.
"""

import os
import unittest
from datetime import date

from common.first_found_index import FirstFoundIndex


class HappyPathBase(unittest.TestCase):
    DATA_DIR: str = ""
    PREFIX: str = "found_issues"

    @classmethod
    def setUpClass(cls):
        if cls is HappyPathBase:
            raise unittest.SkipTest("Base class")

    def setUp(self):
        self.idx = FirstFoundIndex(os.path.join(self.DATA_DIR, self.PREFIX))

    def test_single_example(self):
        self.assertEqual(self.idx.lookup("alpha"), date(2025, 7, 19))

    def test_multiple_examples_earliest_wins(self):
        self.assertEqual(self.idx.lookup("beta"), date(2025, 3, 4))

    def test_separate_tool_dirs(self):
        self.assertEqual(self.idx.lookup("delta"), date(2026, 2, 15))

    def test_missing_id_raises(self):
        with self.assertRaises(KeyError):
            self.idx.lookup("not_present")


class ErrorsBase(unittest.TestCase):
    DATA_DIR: str = ""
    PREFIX: str = "found_issues"

    @classmethod
    def setUpClass(cls):
        if cls is ErrorsBase:
            raise unittest.SkipTest("Base class")

    def _path(self, suffix: str) -> str:
        return os.path.join(self.DATA_DIR, f"{self.PREFIX}_{suffix}")

    def test_missing_first_found_raises(self):
        with self.assertRaises(ValueError) as ctx:
            FirstFoundIndex(self._path("missing_first_found"))
        self.assertIn("no parseable first_found", str(ctx.exception))

    def test_unparsable_first_found_raises(self):
        with self.assertRaises(ValueError) as ctx:
            FirstFoundIndex(self._path("unparsable"))
        self.assertIn("unparsable first_found", str(ctx.exception))

    def test_duplicate_id_across_yaml_files_raises(self):
        with self.assertRaises(ValueError) as ctx:
            FirstFoundIndex(self._path("duplicate"))
        self.assertIn("duplicate error_id", str(ctx.exception))


def make_first_found_index_test_classes(
    data_dir: str,
    prefix: str,
) -> tuple[type[HappyPathBase], type[ErrorsBase]]:
    """Build concrete TestCase subclasses bound to ``data_dir``/``prefix``."""
    happy = type(
        "TestFirstFoundIndexHappyPath",
        (HappyPathBase,),
        {"DATA_DIR": data_dir, "PREFIX": prefix},
    )
    errors = type(
        "TestFirstFoundIndexErrors",
        (ErrorsBase,),
        {"DATA_DIR": data_dir, "PREFIX": prefix},
    )
    return happy, errors
