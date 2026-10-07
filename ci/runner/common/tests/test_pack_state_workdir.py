"""Preserve the project path recorded by upload-pack-state."""

import subprocess
import sys
import tarfile
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[3] / "scripts" / "pack_state_workdir.py"


class TestPackStateWorkdir(unittest.TestCase):
    def test_archived_project_path(self):
        for prefix in ("", "/"):
            for build_dir in (".build", "nested/build"):
                with self.subTest(prefix=prefix, build_dir=build_dir):
                    result = self.run_archive(
                        f"{prefix}__w/verilog-model/verilog-model/{build_dir}/",
                        build_dir,
                    )
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(result.stdout.strip(), "/__w/verilog-model/verilog-model")

    def test_missing_or_wrong_build_directory(self):
        for name in (None, "root/.config/pack/"):
            with self.subTest(name=name):
                result = self.run_archive(name, ".build")
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("Expected the archived build directory", result.stderr)

    @staticmethod
    def run_archive(name, build_dir):
        with tempfile.TemporaryDirectory() as temp:
            archive_path = Path(temp) / "state.tar"
            with tarfile.open(archive_path, "w") as archive:
                if name is not None:
                    entry = tarfile.TarInfo(name)
                    entry.type = tarfile.DIRTYPE
                    archive.addfile(entry)
            return subprocess.run(
                [sys.executable, str(SCRIPT), str(archive_path), build_dir],
                capture_output=True,
                text=True,
                check=False,
            )


if __name__ == "__main__":
    unittest.main()
