import contextlib
import tempfile
import unittest
from pathlib import Path

from compare_errors.reproduced_index import ReproducedIndex

from ._helpers import make_file
from ._helpers import write_per_file as _write_per_file
from ._helpers import write_yaml


def _write_regression_per_file(dir_path: Path, tool_name: str, suffix: str, files: list) -> None:
    _write_per_file(dir_path, tool_name, files, suffix=suffix, name_prefix="regression-")


@contextlib.contextmanager
def _scenario():
    """Yield (regression_dir, dataset) under a fresh temp directory."""
    with tempfile.TemporaryDirectory() as tmp:
        tmp_path = Path(tmp)
        regression_dir = tmp_path / "regression"
        regression_dir.mkdir()
        dataset = tmp_path / "dataset"
        dataset.mkdir()
        yield regression_dir, dataset


class TestReproducedIndex(unittest.TestCase):
    def test_reproduced_when_any_example_reproduces(self):
        with _scenario() as (regression_dir, dataset):
            write_yaml(dataset, "tool_a", "err_a", "sv", [("ex1", "minified"), ("ex1", "full")])
            _write_regression_per_file(
                regression_dir,
                "tool_a",
                "",
                [
                    make_file("ex1-minified.sv"),  # not reproduced
                    make_file("ex1-full.sv", ("err_a", "boom")),  # reproduced
                ],
            )
            idx = ReproducedIndex(regression_dir, dataset)
        self.assertTrue(idx.reproduced("err_a"))

    def test_not_reproduced_when_no_example_matches(self):
        with _scenario() as (regression_dir, dataset):
            write_yaml(dataset, "tool_a", "err_a", "sv", [("ex1", "minified")])
            _write_regression_per_file(
                regression_dir,
                "tool_a",
                "",
                [
                    make_file("ex1-minified.sv", ("err_b", "wrong")),  # wrong id
                ],
            )
            idx = ReproducedIndex(regression_dir, dataset)
        self.assertFalse(idx.reproduced("err_a"))

    def test_unknown_error_id_returns_false(self):
        with _scenario() as (regression_dir, dataset):
            write_yaml(dataset, "tool_a", "err_a", "sv", [("ex1", "minified")])
            _write_regression_per_file(regression_dir, "tool_a", "", [make_file("ex1-minified.sv")])
            idx = ReproducedIndex(regression_dir, dataset)
        self.assertFalse(idx.reproduced("not_in_dataset"))

    def test_empty_regression_dir_returns_false_for_everything(self):
        with _scenario() as (regression_dir, dataset):
            idx = ReproducedIndex(regression_dir, dataset)
        self.assertFalse(idx.reproduced("anything"))

    def test_files_for_unmapped_pair_skipped(self):
        # An example present in the per-file JSON whose (name, type) is not in
        # the YAML map should not register anywhere.
        with _scenario() as (regression_dir, dataset):
            write_yaml(dataset, "tool_a", "err_a", "sv", [("ex1", "minified")])
            _write_regression_per_file(
                regression_dir,
                "tool_a",
                "",
                [
                    make_file("ghost-minified.sv", ("err_b", "boom")),
                ],
            )
            idx = ReproducedIndex(regression_dir, dataset)
        self.assertFalse(idx.reproduced("err_a"))
        self.assertFalse(idx.reproduced("err_b"))

    def test_multi_tool_isolation(self):
        # Two tools in the same regression dir; each has its own YAML tree.
        # Reproductions in one tool must not leak into the other.
        with _scenario() as (regression_dir, dataset):
            write_yaml(dataset, "tool_a", "err_a", "sv", [("ex1", "minified")])
            write_yaml(dataset, "tool_b", "err_b", "vhdl", [("ex2", "full")])
            _write_regression_per_file(
                regression_dir,
                "tool_a",
                "",
                [
                    make_file("ex1-minified.sv", ("err_a", "boom")),
                ],
            )
            _write_regression_per_file(
                regression_dir,
                "tool_b",
                "",
                [
                    make_file("ex2-full.vhdl"),  # no match -> not reproduced
                ],
            )
            idx = ReproducedIndex(regression_dir, dataset)
        self.assertTrue(idx.reproduced("err_a"))
        self.assertFalse(idx.reproduced("err_b"))

    def test_or_merge_across_multiple_jsons_for_same_tool(self):
        # Two regression JSONs for tool_a; the or-merge must keep True once seen.
        with _scenario() as (regression_dir, dataset):
            write_yaml(dataset, "tool_a", "err_a", "sv", [("ex1", "minified")])
            _write_regression_per_file(
                regression_dir,
                "tool_a",
                "-batch1",
                [
                    make_file("ex1-minified.sv"),  # not reproduced
                ],
            )
            _write_regression_per_file(
                regression_dir,
                "tool_a",
                "-batch2",
                [
                    make_file("ex1-minified.sv", ("err_a", "boom")),  # reproduced
                ],
            )
            idx = ReproducedIndex(regression_dir, dataset)
        self.assertTrue(idx.reproduced("err_a"))
