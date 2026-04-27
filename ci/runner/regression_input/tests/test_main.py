# ci/runner/regression_input/tests/test_main.py
import shutil
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from regression_input.main import main as regression_input_main

SAMPLE_YAML_SV = """\
id: err_a
target: iverilog
regex: 'error: a'
title: 'A'
profile: sv
examples:
  - foo:
      minified_example: |
        module a; endmodule
      full_example: |
        module a;
          // longer
        endmodule
"""

SAMPLE_YAML_VHDL = """\
id: err_b
target: ghdl
regex: 'error: b'
title: 'B'
profile: vhdl
examples:
  - bar:
      minified_example: |
        entity b is end;
"""


class TestRegressionInputMain(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp())
        self.known = self.tmp / "found_issues"
        (self.known / "iverilog").mkdir(parents=True)
        (self.known / "ghdl").mkdir(parents=True)
        (self.known / "iverilog" / "err_a.yaml").write_text(SAMPLE_YAML_SV)
        (self.known / "ghdl" / "err_b.yaml").write_text(SAMPLE_YAML_VHDL)
        self.lang_config = self.tmp / "languages.yaml"
        self.lang_config.write_text("sv: .sv\nvhdl: .vhdl\n")
        self.out = self.tmp / "out"

    def tearDown(self):
        shutil.rmtree(self.tmp)

    def _run(self, language: str) -> int:
        argv = [
            "regression_input.main",
            "--known-errors-dir",
            str(self.known),
            "--language",
            language,
            "--language-config",
            str(self.lang_config),
            "--out-dir",
            str(self.out),
        ]
        with patch.object(sys, "argv", argv):
            try:
                regression_input_main()
            except SystemExit as e:
                return int(e.code or 0)
            return 0

    def test_sv_materialises_two_variants(self):
        rc = self._run("sv")
        self.assertEqual(rc, 0)
        names = sorted(p.name for p in self.out.iterdir())
        self.assertEqual(names, ["foo-full.sv", "foo-minified.sv"])
        self.assertIn("module a;", (self.out / "foo-minified.sv").read_text())

    def test_vhdl_filters_by_language(self):
        rc = self._run("vhdl")
        self.assertEqual(rc, 0)
        names = sorted(p.name for p in self.out.iterdir())
        self.assertEqual(names, ["bar-minified.vhdl"])


if __name__ == "__main__":
    unittest.main()
