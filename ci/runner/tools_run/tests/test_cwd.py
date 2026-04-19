import os
import tempfile
import unittest

from common.run_command import run_command


class TestRunCommandCwd(unittest.TestCase):
    def test_cwd_execution(self):
        with tempfile.TemporaryDirectory() as tmp_dir:
            run_command("touch test_artifact", cwd=tmp_dir)

            # Check if file exists in tmp_dir
            self.assertTrue(os.path.exists(os.path.join(tmp_dir, "test_artifact")))
            # Check if file does NOT exist in current dir (if distinct)
            self.assertFalse(os.path.exists("test_artifact"))
