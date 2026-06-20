"""Tests for the repo-side per-profile translate hooks (ci/scripts/*.py)."""

import subprocess
import tempfile
import unittest
from pathlib import Path

# common/tests/ -> common -> runner -> ci
_CI_DIR = Path(__file__).resolve().parents[3]
SV_HOOK = _CI_DIR / "scripts" / "sv.py"
VHDL_HOOK = _CI_DIR / "scripts" / "vhdl.py"


def _run_hook(hook: Path, template: str, content: str):
    with tempfile.NamedTemporaryFile("w", delete=False) as f:
        f.write(content)
        path = f.name
    proc = subprocess.run(
        ["python3", str(hook), template, path],
        capture_output=True,
        text=True,
        check=False,
    )
    return proc, path


class TestSvHook(unittest.TestCase):

    def test_file_placeholder(self):
        proc, path = _run_hook(SV_HOOK, "run {file}", "module m; endmodule")
        self.assertEqual(proc.returncode, 0, proc.stderr)
        self.assertEqual(proc.stdout, f"run {path}")

    def test_top_module(self):
        proc, _ = _run_hook(SV_HOOK, "sim {top_module}", "module my_mod; endmodule")
        self.assertEqual(proc.returncode, 0, proc.stderr)
        self.assertEqual(proc.stdout, "sim my_mod")

    def test_returns_last_module(self):
        proc, _ = _run_hook(SV_HOOK, "{top_module}", "module first; endmodule\nmodule last; endmodule")
        self.assertEqual(proc.stdout, "last")

    def test_digit_in_name(self):
        proc, _ = _run_hook(SV_HOOK, "{top_module}", "module tb_1; endmodule")
        self.assertEqual(proc.stdout, "tb_1")

    def test_no_module_with_placeholder_fails(self):
        proc, _ = _run_hook(SV_HOOK, "{top_module}", "// just a comment\n")
        self.assertNotEqual(proc.returncode, 0)

    def test_no_module_lookup_when_placeholder_absent(self):
        proc, path = _run_hook(SV_HOOK, "echo hi {file}", "// no module here")
        self.assertEqual(proc.returncode, 0, proc.stderr)
        self.assertEqual(proc.stdout, f"echo hi {path}")


class TestVhdlHook(unittest.TestCase):

    def test_file_placeholder(self):
        proc, path = _run_hook(VHDL_HOOK, "run {file}", "entity e is\nend e;")
        self.assertEqual(proc.returncode, 0, proc.stderr)
        self.assertEqual(proc.stdout, f"run {path}")

    def test_top_entity_uppercase(self):
        proc, _ = _run_hook(VHDL_HOOK, "ghdl -e {vhdl_top_entity}", "ENTITY top IS\nEND top;")
        self.assertEqual(proc.returncode, 0, proc.stderr)
        self.assertEqual(proc.stdout, "ghdl -e top")

    def test_mixed_case_preserved(self):
        proc, _ = _run_hook(VHDL_HOOK, "{vhdl_top_entity}", "Entity Foo Is\nEnd Foo;")
        self.assertEqual(proc.stdout, "Foo")

    def test_returns_last_entity(self):
        proc, _ = _run_hook(VHDL_HOOK, "{vhdl_top_entity}", "entity first is\nend first;\nentity last is\nend last;")
        self.assertEqual(proc.stdout, "last")

    def test_underscore_and_digit(self):
        proc, _ = _run_hook(VHDL_HOOK, "{vhdl_top_entity}", "entity tb_1 is\nend tb_1;")
        self.assertEqual(proc.stdout, "tb_1")

    def test_no_entity_with_placeholder_fails(self):
        proc, _ = _run_hook(VHDL_HOOK, "{vhdl_top_entity}", "-- no entity here\n")
        self.assertNotEqual(proc.returncode, 0)


if __name__ == "__main__":
    unittest.main()
