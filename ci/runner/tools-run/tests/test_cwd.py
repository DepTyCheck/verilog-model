import unittest
import os
import tempfile
from pathlib import Path
from src.run_command import RunCommand


class TestRunCommandCwd(unittest.TestCase):
    def test_cwd_execution(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            # Command that creates a file in CWD
            cmd = RunCommand(
                raw_str_cmd="touch test_artifact",
                file_path="dummy",
                file_content="",
                cwd=tmp_dir,
            )
            cmd.execute()

            # Check if file exists in tmp_dir
            self.assertTrue(os.path.exists(os.path.join(tmp_dir, "test_artifact")))
            # Check if file does NOT exist in current dir (if distinct)
            self.assertFalse(os.path.exists("test_artifact"))
