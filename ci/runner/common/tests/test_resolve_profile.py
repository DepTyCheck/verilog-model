"""Tests for the repo-side ci/scripts/resolve_profile.py."""

import subprocess
import tempfile
import unittest
from pathlib import Path

# common/tests/ -> common -> runner -> ci
_CI_DIR = Path(__file__).resolve().parents[3]
SCRIPT = _CI_DIR / "scripts" / "resolve_profile.py"

_VALID = "sv:\n  file_extension: .sv\n  translate_hook: ci/scripts/sv.py\n"


def _write(body: str) -> str:
    with tempfile.NamedTemporaryFile("w", suffix=".yaml", delete=False) as f:
        f.write(body)
        return f.name


def _run(profiles_path: str, profile: str):
    return subprocess.run(
        ["python3", str(SCRIPT), "--profiles", profiles_path, "--profile", profile],
        capture_output=True,
        text=True,
        check=False,
    )


class TestResolveProfile(unittest.TestCase):

    def test_valid_profile(self):
        proc = _run(_write(_VALID), "sv")
        self.assertEqual(proc.returncode, 0, proc.stderr)
        self.assertIn("TRANSLATE_HOOK=ci/scripts/sv.py", proc.stdout)
        self.assertIn("FILE_PATTERN=*.sv", proc.stdout)

    def test_unknown_profile_fails(self):
        proc = _run(_write(_VALID), "vhdl")
        self.assertNotEqual(proc.returncode, 0)

    def test_missing_option_fails(self):
        proc = _run(_write("sv:\n  file_extension: .sv\n"), "sv")  # no translate_hook
        self.assertNotEqual(proc.returncode, 0)


if __name__ == "__main__":
    unittest.main()
