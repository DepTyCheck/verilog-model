import tempfile
import unittest
from pathlib import Path
from textwrap import dedent

from regression_analyze.expected import build_expected


def _write_yaml(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(dedent(content), encoding="utf-8")


class TestBuildExpected(unittest.TestCase):
    def test_collects_examples_for_named_tool(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            _write_yaml(
                root / "iverilog" / "err_a.yaml",
                """
                id: err_a
                target: iverilog
                regex: "boom"
                title: example
                profile: sv
                examples:
                  - foo:
                      minified_example: "x"
                      full_example: "xx"
                """,
            )
            result = build_expected(str(root), "iverilog")
            self.assertEqual(
                result,
                {("foo", "minified"): "err_a", ("foo", "full"): "err_a"},
            )

    def test_other_tools_ignored(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            _write_yaml(
                root / "verilator" / "err_b.yaml",
                """
                id: err_b
                target: verilator
                regex: "boom"
                title: example
                profile: sv
                examples:
                  - bar:
                      minified_example: "y"
                """,
            )
            result = build_expected(str(root), "iverilog")
            self.assertEqual(result, {})

    def test_missing_dir_returns_empty(self):
        result = build_expected("/nonexistent/path", "iverilog")
        self.assertEqual(result, {})


if __name__ == "__main__":
    unittest.main()
