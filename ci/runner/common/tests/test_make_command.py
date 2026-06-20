"""Tests for common/make_command.py — the hook delegation contract."""

import tempfile
import unittest
from pathlib import Path

from common.make_command import make_command


def _write_hook(dir_path: str, body: str) -> str:
    hook = Path(dir_path) / "hook.py"
    hook.write_text("import sys\n" + body)
    return str(hook)


class TestMakeCommand(unittest.TestCase):

    def test_returns_hook_stdout(self):
        with tempfile.TemporaryDirectory() as d:
            # argv[1] = template, argv[2] = file path
            hook = _write_hook(d, 'sys.stdout.write("TOOL " + sys.argv[2])\n')
            self.assertEqual(make_command("ignored", "/tmp/f.x", hook), "TOOL /tmp/f.x")

    def test_passes_template_as_first_arg(self):
        with tempfile.TemporaryDirectory() as d:
            hook = _write_hook(d, "sys.stdout.write(sys.argv[1])\n")
            self.assertEqual(make_command("run {file}", "/tmp/f.x", hook), "run {file}")

    def test_strips_trailing_newline(self):
        with tempfile.TemporaryDirectory() as d:
            hook = _write_hook(d, 'sys.stdout.write("cmd\\n")\n')
            self.assertEqual(make_command("t", "/tmp/f", hook), "cmd")

    def test_hook_nonzero_exit_raises(self):
        with tempfile.TemporaryDirectory() as d:
            hook = _write_hook(d, 'sys.stderr.write("boom")\nsys.exit(1)\n')
            with self.assertRaises(Exception):
                make_command("t", "/tmp/f", hook)


if __name__ == "__main__":
    unittest.main()
