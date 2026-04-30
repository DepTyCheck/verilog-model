# ci/runner/runner/tests/test_main.py
import json
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from common.run_command import ExecutionResult
from runner.main import main as runner_main


def _exec(ok: bool, output: str = "") -> ExecutionResult:
    return ExecutionResult(command_executed_successfully=True, result_code_is_ok=ok, timed_out=False, output=output)


class TestRunnerMain(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp())
        self.input_dir = self.tmp / "input"
        self.input_dir.mkdir()
        (self.input_dir / "a.sv").write_text("module a; endmodule\n")
        (self.input_dir / "b.sv").write_text("module b; endmodule\n")
        self.ignored_dir = self.tmp / "ignored"
        self.ignored_dir.mkdir()  # empty: no known errors registered
        self.out = self.tmp / "out.json"

    def tearDown(self):
        shutil.rmtree(self.tmp)

    def _run(self, exec_results: list[ExecutionResult]) -> int:
        argv = [
            "runner.main",
            "--input-dir",
            str(self.input_dir),
            "--file-pattern",
            "*.sv",
            "--tool-name",
            "stub",
            "--tool-version",
            "1.0",
            "--tool-commit",
            "tcommit",
            "--model-commit",
            "mcommit",
            "--commands-json",
            json.dumps([{"run": "echo {file}", "error_regex": None}]),
            "--ignored-errors-dir",
            str(self.ignored_dir),
            "--output",
            str(self.out),
        ]
        with patch.object(sys, "argv", argv), patch("common.run_command.subprocess.run") as mock_run:
            # Each call returns the next ExecutionResult — mock subprocess directly.
            iterator = iter(exec_results)

            class _Completed:
                def __init__(self, ok, output):
                    self.returncode = 0 if ok else 1
                    self.stdout = output

            def _side_effect(*_args, **_kwargs):
                er = next(iterator)
                return _Completed(er.result_code_is_ok, er.output)

            mock_run.side_effect = _side_effect
            try:
                runner_main()
            except SystemExit as exc:
                return int(exc.code or 0)
            return 0

    def test_all_clean_exits_zero(self):
        exit_code = self._run([_exec(ok=True), _exec(ok=True)])
        self.assertEqual(exit_code, 0)
        data = json.loads(self.out.read_text())
        self.assertEqual(data["tool_name"], "stub")
        self.assertEqual(len(data["files"]), 2)
        for f in data["files"]:
            self.assertEqual(len(f["commands"]), 1)
            self.assertEqual(f["commands"][0]["outcome"], "clean")

    def test_unknown_exits_one(self):
        exit_code = self._run([_exec(ok=False, output="boom"), _exec(ok=True)])
        self.assertEqual(exit_code, 1)
        data = json.loads(self.out.read_text())
        outcomes = sorted(c["outcome"] for f in data["files"] for c in f["commands"])
        self.assertIn("unknown", outcomes)

    def test_missing_input_dir_exits_zero(self):
        # Point at a non-existent directory; runner should warn, write empty report, exit 0.
        bogus = self.tmp / "does_not_exist"
        argv = [
            "runner.main",
            "--input-dir",
            str(bogus),
            "--file-pattern",
            "*.sv",
            "--tool-name",
            "stub",
            "--tool-version",
            "1.0",
            "--tool-commit",
            "tcommit",
            "--model-commit",
            "mcommit",
            "--commands-json",
            json.dumps([{"run": "echo {file}", "error_regex": None}]),
            "--ignored-errors-dir",
            str(self.ignored_dir),
            "--output",
            str(self.out),
        ]
        with patch.object(sys, "argv", argv):
            try:
                runner_main()
                exit_code = 0
            except SystemExit as exc:
                exit_code = int(exc.code or 0)
        self.assertEqual(exit_code, 0)
        data = json.loads(self.out.read_text())
        self.assertEqual(data["files"], [])

    def test_empty_input_dir_exits_zero(self):
        # Existing dir with no matching files; runner should warn, write empty report, exit 0.
        empty = self.tmp / "empty_input"
        empty.mkdir()
        argv = [
            "runner.main",
            "--input-dir",
            str(empty),
            "--file-pattern",
            "*.sv",
            "--tool-name",
            "stub",
            "--tool-version",
            "1.0",
            "--tool-commit",
            "tcommit",
            "--model-commit",
            "mcommit",
            "--commands-json",
            json.dumps([{"run": "echo {file}", "error_regex": None}]),
            "--ignored-errors-dir",
            str(self.ignored_dir),
            "--output",
            str(self.out),
        ]
        with patch.object(sys, "argv", argv):
            try:
                runner_main()
                exit_code = 0
            except SystemExit as exc:
                exit_code = int(exc.code or 0)
        self.assertEqual(exit_code, 0)
        data = json.loads(self.out.read_text())
        self.assertEqual(data["files"], [])

    def test_timeout_does_not_fail_job(self):
        # subprocess.run raising TimeoutExpired must classify as outcome="timeout" (not "unknown")
        # and runner must exit 0 — timeouts do NOT fail the job.
        argv = [
            "runner.main",
            "--input-dir",
            str(self.input_dir),
            "--file-pattern",
            "*.sv",
            "--tool-name",
            "stub",
            "--tool-version",
            "1.0",
            "--tool-commit",
            "tcommit",
            "--model-commit",
            "mcommit",
            "--commands-json",
            json.dumps([{"run": "echo {file}", "error_regex": None}]),
            "--ignored-errors-dir",
            str(self.ignored_dir),
            "--output",
            str(self.out),
        ]
        with patch.object(sys, "argv", argv), patch("common.run_command.subprocess.run") as mock_run:
            mock_run.side_effect = subprocess.TimeoutExpired(cmd="stub", timeout=1)
            try:
                runner_main()
                exit_code = 0
            except SystemExit as exc:
                exit_code = int(exc.code or 0)
        self.assertEqual(exit_code, 0)
        data = json.loads(self.out.read_text())
        outcomes = [c["outcome"] for f in data["files"] for c in f["commands"]]
        self.assertEqual(outcomes, ["timeout", "timeout"])


if __name__ == "__main__":
    unittest.main()
