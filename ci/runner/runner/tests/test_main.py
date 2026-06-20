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

# runner/tests/ -> runner -> ci/runner -> ci
_SV_HOOK = Path(__file__).resolve().parents[3] / "scripts" / "sv.py"


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

    def _argv(self, input_dir: Path) -> list[str]:
        return [
            "runner.main",
            "--input-dir",
            str(input_dir),
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
            "--translate-hook",
            str(_SV_HOOK),
            "--output",
            str(self.out),
        ]

    def _invoke(self) -> int:
        try:
            runner_main()
        except SystemExit as exc:
            return int(exc.code or 0)
        return 0

    def _run(self, exec_results: list[ExecutionResult]) -> int:
        argv = self._argv(self.input_dir)
        _real_subprocess_run = subprocess.run
        with patch.object(sys, "argv", argv), patch("common.run_command.subprocess.run") as mock_run:
            # Each call returns the next ExecutionResult — mock subprocess directly.
            # make_command also uses subprocess.run (list arg + capture_output=True);
            # delegate those to the real subprocess so sv.py executes correctly.
            iterator = iter(exec_results)

            class _Completed:
                def __init__(self, ok, output):
                    self.returncode = 0 if ok else 1
                    self.stdout = output

            def _side_effect(*args, **kwargs):
                # make_command passes a list; run_command passes a shell string
                if args and isinstance(args[0], list):
                    return _real_subprocess_run(*args, check=kwargs.pop("check", False), **kwargs)
                er = next(iterator)
                return _Completed(er.result_code_is_ok, er.output)

            mock_run.side_effect = _side_effect
            return self._invoke()

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
        with patch.object(sys, "argv", self._argv(bogus)):
            exit_code = self._invoke()
        self.assertEqual(exit_code, 0)
        data = json.loads(self.out.read_text())
        self.assertEqual(data["files"], [])

    def test_empty_input_dir_exits_zero(self):
        # Existing dir with no matching files; runner should warn, write empty report, exit 0.
        empty = self.tmp / "empty_input"
        empty.mkdir()
        with patch.object(sys, "argv", self._argv(empty)):
            exit_code = self._invoke()
        self.assertEqual(exit_code, 0)
        data = json.loads(self.out.read_text())
        self.assertEqual(data["files"], [])

    def test_timeout_does_not_fail_job(self):
        # subprocess.run raising TimeoutExpired must classify as outcome="timeout" (not "unknown")
        # and runner must exit 0 — timeouts do NOT fail the job.
        argv = self._argv(self.input_dir)
        _real_subprocess_run = subprocess.run
        with patch.object(sys, "argv", argv), patch("common.run_command.subprocess.run") as mock_run:

            def _timeout_side_effect(*args, **kwargs):
                # make_command passes a list; let sv.py run for real
                if args and isinstance(args[0], list):
                    return _real_subprocess_run(*args, check=kwargs.pop("check", False), **kwargs)
                raise subprocess.TimeoutExpired(cmd="stub", timeout=1)

            mock_run.side_effect = _timeout_side_effect
            exit_code = self._invoke()
        self.assertEqual(exit_code, 0)
        data = json.loads(self.out.read_text())
        outcomes = [c["outcome"] for f in data["files"] for c in f["commands"]]
        self.assertEqual(outcomes, ["timeout", "timeout"])

    def test_file_pattern_without_suffix_exits_two(self):
        argv = self._argv(self.input_dir)
        i = argv.index("--file-pattern")
        argv[i + 1] = "*"  # glob with no extension
        with patch.object(sys, "argv", argv):
            exit_code = self._invoke()
        self.assertEqual(exit_code, 2)


if __name__ == "__main__":
    unittest.main()
