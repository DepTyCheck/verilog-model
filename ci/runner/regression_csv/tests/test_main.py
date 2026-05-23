import tempfile
import unittest
from pathlib import Path
from textwrap import dedent
from unittest.mock import patch

from regression_csv.main import main
from regression_csv.tests.helpers import clean_file, known_error_file, write_artifact


def _write_yaml(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(dedent(content), encoding="utf-8")


def _run(regression_dir: Path, known_errors_dir: Path, out_csv: Path) -> None:
    argv = [
        "prog",
        "--regression-results-dir",
        str(regression_dir),
        "--known-errors-dir",
        str(known_errors_dir),
        "--out-csv",
        str(out_csv),
    ]
    with patch("sys.argv", argv):
        main()


class TestRegressionCsvMain(unittest.TestCase):
    def test_writes_csv_for_single_tool(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            reg = root / "regression"
            known = root / "found_issues"
            out = root / "latest_regression_test.csv"
            _write_yaml(
                known / "iverilog" / "err_a.yaml",
                """
                id: err_a
                target: iverilog
                regex: "boom"
                title: ex
                profile: sv
                examples:
                  - foo:
                      minified_example: "x"
                      full_example: "xx"
                """,
            )
            write_artifact(
                reg / "regression-iverilog-per-file.json",
                "iverilog",
                [
                    known_error_file("foo-minified.sv", "err_a", "boom"),
                    clean_file("foo-full.sv"),
                ],
            )
            _run(reg, known, out)
            self.assertEqual(
                out.read_text(encoding="utf-8"),
                "example_id,type,is_reproduced\nfoo,full,false\nfoo,minified,true\n",
            )

    def test_multi_tool_rows_sorted_by_example_then_type(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            reg = root / "regression"
            known = root / "found_issues"
            out = root / "latest_regression_test.csv"
            # verilator owns alpha (sorts FIRST), iverilog owns beta (sorts SECOND)
            # Artifact glob order is iverilog → verilator (filename alpha-order),
            # so the sort step must actually reorder the rows.
            _write_yaml(
                known / "iverilog" / "err_b.yaml",
                """
                id: err_b
                target: iverilog
                regex: "boom"
                title: ex
                profile: sv
                examples:
                  - beta:
                      full_example: "y"
                """,
            )
            _write_yaml(
                known / "verilator" / "err_a.yaml",
                """
                id: err_a
                target: verilator
                regex: "boom"
                title: ex
                profile: sv
                examples:
                  - alpha:
                      minified_example: "x"
                """,
            )
            write_artifact(
                reg / "regression-iverilog-per-file.json",
                "iverilog",
                [clean_file("beta-full.sv")],
            )
            write_artifact(
                reg / "regression-verilator-per-file.json",
                "verilator",
                [known_error_file("alpha-minified.sv", "err_a", "boom")],
            )
            _run(reg, known, out)
            self.assertEqual(
                out.read_text(encoding="utf-8"),
                "example_id,type,is_reproduced\nalpha,minified,true\nbeta,full,false\n",
            )

    def test_no_artifacts_skips_write(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            reg = root / "regression"
            reg.mkdir()
            known = root / "found_issues"
            known.mkdir()
            out = root / "latest_regression_test.csv"
            _run(reg, known, out)
            self.assertFalse(out.exists())

    def test_missing_regression_dir_skips_write(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            reg = root / "regression"  # not created
            known = root / "found_issues"
            known.mkdir()
            out = root / "latest_regression_test.csv"
            _run(reg, known, out)
            self.assertFalse(out.exists())

    def test_tool_with_no_known_errors_yields_no_rows(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            reg = root / "regression"
            known = root / "found_issues"
            known.mkdir()
            out = root / "latest_regression_test.csv"
            write_artifact(
                reg / "regression-mystery-per-file.json",
                "mystery",
                [clean_file("foo-minified.sv")],
            )
            _run(reg, known, out)
            self.assertFalse(out.exists())


if __name__ == "__main__":
    unittest.main()
