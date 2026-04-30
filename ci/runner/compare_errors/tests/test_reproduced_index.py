import json
import tempfile
import textwrap
import unittest
from pathlib import Path

from compare_errors.reproduced_index import ReproducedIndex


def _write_per_file(dir_path: Path, tool_name: str, suffix: str, files: list) -> None:
    payload = {
        "tool_name": tool_name,
        "tool_version": "v",
        "tool_commit": "c",
        "model_commit": "m",
        "run_date": "2026-04-30",
        "files": files,
    }
    name = f"regression-{tool_name}{suffix}-per-file.json"
    (dir_path / name).write_text(json.dumps(payload), encoding="utf-8")


def _file(filename: str, *match_pairs: tuple[str, str]) -> dict:
    return {
        "filename": filename,
        "commands": [
            {
                "command": "tool args",
                "outcome": "known_errors" if match_pairs else "clean",
                "matches": [{"error_id": eid, "matched_text": text} for eid, text in match_pairs],
            }
        ],
    }


def _write_yaml(dataset: Path, tool: str, error_id: str, language: str, examples: list[tuple[str, str]]) -> None:
    yaml_dir = dataset / "found_issues" / tool
    yaml_dir.mkdir(parents=True, exist_ok=True)
    blocks = []
    for name, kind in examples:
        blocks.append(f"  - {name}:\n      {kind}_example: |\n        // body\n      first_found: 01.01.2026")
    body = textwrap.dedent(f"""
        id: {error_id}
        target: {tool}
        regex: "boom"
        title: "title"
        profile: {language}
        examples:
        """).strip() + "\n" + "\n".join(blocks) + "\n"
    (yaml_dir / f"{error_id}.yaml").write_text(body, encoding="utf-8")


class TestReproducedIndex(unittest.TestCase):
    def test_reproduced_when_any_example_reproduces(self):
        with tempfile.TemporaryDirectory() as tmp:
            tmp_path = Path(tmp)
            regression_dir = tmp_path / "regression"
            regression_dir.mkdir()
            dataset = tmp_path / "dataset"
            dataset.mkdir()
            _write_yaml(dataset, "tool_a", "err_a", "sv", [("ex1", "minified"), ("ex1", "full")])
            _write_per_file(
                regression_dir,
                "tool_a",
                "",
                [
                    _file("ex1-minified.sv"),  # not reproduced
                    _file("ex1-full.sv", ("err_a", "boom")),  # reproduced
                ],
            )
            idx = ReproducedIndex(regression_dir, dataset)
        self.assertTrue(idx.reproduced("err_a"))

    def test_not_reproduced_when_no_example_matches(self):
        with tempfile.TemporaryDirectory() as tmp:
            tmp_path = Path(tmp)
            regression_dir = tmp_path / "regression"
            regression_dir.mkdir()
            dataset = tmp_path / "dataset"
            dataset.mkdir()
            _write_yaml(dataset, "tool_a", "err_a", "sv", [("ex1", "minified")])
            _write_per_file(
                regression_dir,
                "tool_a",
                "",
                [
                    _file("ex1-minified.sv", ("err_b", "wrong")),  # wrong id
                ],
            )
            idx = ReproducedIndex(regression_dir, dataset)
        self.assertFalse(idx.reproduced("err_a"))

    def test_unknown_error_id_returns_false(self):
        with tempfile.TemporaryDirectory() as tmp:
            tmp_path = Path(tmp)
            regression_dir = tmp_path / "regression"
            regression_dir.mkdir()
            dataset = tmp_path / "dataset"
            dataset.mkdir()
            _write_yaml(dataset, "tool_a", "err_a", "sv", [("ex1", "minified")])
            _write_per_file(regression_dir, "tool_a", "", [_file("ex1-minified.sv")])
            idx = ReproducedIndex(regression_dir, dataset)
        self.assertFalse(idx.reproduced("not_in_dataset"))

    def test_empty_regression_dir_returns_false_for_everything(self):
        with tempfile.TemporaryDirectory() as tmp:
            tmp_path = Path(tmp)
            regression_dir = tmp_path / "regression"
            regression_dir.mkdir()
            dataset = tmp_path / "dataset"
            dataset.mkdir()
            idx = ReproducedIndex(regression_dir, dataset)
        self.assertFalse(idx.reproduced("anything"))

    def test_files_for_unmapped_pair_skipped(self):
        # An example present in the per-file JSON whose (name, type) is not in
        # the YAML map should not register anywhere.
        with tempfile.TemporaryDirectory() as tmp:
            tmp_path = Path(tmp)
            regression_dir = tmp_path / "regression"
            regression_dir.mkdir()
            dataset = tmp_path / "dataset"
            dataset.mkdir()
            _write_yaml(dataset, "tool_a", "err_a", "sv", [("ex1", "minified")])
            _write_per_file(
                regression_dir,
                "tool_a",
                "",
                [
                    _file("ghost-minified.sv", ("err_b", "boom")),
                ],
            )
            idx = ReproducedIndex(regression_dir, dataset)
        self.assertFalse(idx.reproduced("err_a"))
        self.assertFalse(idx.reproduced("err_b"))

    def test_multi_tool_isolation(self):
        # Two tools in the same regression dir; each has its own YAML tree.
        # Reproductions in one tool must not leak into the other.
        with tempfile.TemporaryDirectory() as tmp:
            tmp_path = Path(tmp)
            regression_dir = tmp_path / "regression"
            regression_dir.mkdir()
            dataset = tmp_path / "dataset"
            dataset.mkdir()
            _write_yaml(dataset, "tool_a", "err_a", "sv", [("ex1", "minified")])
            _write_yaml(dataset, "tool_b", "err_b", "vhdl", [("ex2", "full")])
            _write_per_file(
                regression_dir,
                "tool_a",
                "",
                [
                    _file("ex1-minified.sv", ("err_a", "boom")),
                ],
            )
            _write_per_file(
                regression_dir,
                "tool_b",
                "",
                [
                    _file("ex2-full.vhdl"),  # no match -> not reproduced
                ],
            )
            idx = ReproducedIndex(regression_dir, dataset)
        self.assertTrue(idx.reproduced("err_a"))
        self.assertFalse(idx.reproduced("err_b"))

    def test_or_merge_across_multiple_jsons_for_same_tool(self):
        # Two regression JSONs for tool_a; the or-merge must keep True once seen.
        with tempfile.TemporaryDirectory() as tmp:
            tmp_path = Path(tmp)
            regression_dir = tmp_path / "regression"
            regression_dir.mkdir()
            dataset = tmp_path / "dataset"
            dataset.mkdir()
            _write_yaml(dataset, "tool_a", "err_a", "sv", [("ex1", "minified")])
            _write_per_file(
                regression_dir,
                "tool_a",
                "-batch1",
                [
                    _file("ex1-minified.sv"),  # not reproduced
                ],
            )
            _write_per_file(
                regression_dir,
                "tool_a",
                "-batch2",
                [
                    _file("ex1-minified.sv", ("err_a", "boom")),  # reproduced
                ],
            )
            idx = ReproducedIndex(regression_dir, dataset)
        self.assertTrue(idx.reproduced("err_a"))
