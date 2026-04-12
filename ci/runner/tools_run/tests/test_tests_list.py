"""Unit tests for tools_run/src/tests_list.py — CommandConfig and N-step pipeline."""

import unittest
from unittest.mock import MagicMock, patch

from common.command_output import AnalyzisResult
from common.error_types import UnexpectedError
from common.run_command import ExecutionResult
from common.tool_error_regex import ToolErrorRegex
from tools_run.src.tests_list import CommandConfig, RunStatsCounter, TestsList


def _clean_result() -> AnalyzisResult:
    return AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)


def _unexpected_result(path: str = "file.sv") -> AnalyzisResult:
    return AnalyzisResult(
        found_matches=[],
        unexpected_errors=[UnexpectedError(tool_output_error_text="boom", test_file_path=path)],
        all_errors_are_known=False,
    )


def _exec_result(ok: bool, output: str = "") -> ExecutionResult:
    return ExecutionResult(
        command_executed_successfully=True,
        result_code_is_ok=ok,
        timed_out=False,
        output=output,
    )


# ---------------------------------------------------------------------------
# CommandConfig
# ---------------------------------------------------------------------------


class TestCommandConfig(unittest.TestCase):

    def test_run_required(self):
        cc = CommandConfig(run="tool {file}")
        self.assertEqual(cc.run, "tool {file}")

    def test_error_regex_defaults_to_none(self):
        cc = CommandConfig(run="tool {file}")
        self.assertIsNone(cc.error_regex)

    def test_error_regex_can_be_set(self):
        regex = ToolErrorRegex("error: .*")
        cc = CommandConfig(run="tool {file}", error_regex=regex)
        self.assertIs(cc.error_regex, regex)


# ---------------------------------------------------------------------------
# RunStatsCounter
# ---------------------------------------------------------------------------


class TestRunStatsCounter(unittest.TestCase):

    def _counter(self):
        return RunStatsCounter()

    def test_all_commands_succeed_gives_clean(self):
        c = self._counter()
        c.resolve([True, True], [_clean_result(), _clean_result()])
        self.assertEqual(c.run_stats["clean"], 1)
        self.assertEqual(c.run_stats["handled_errors"], 0)
        self.assertEqual(c.run_stats["failed"], 0)

    def test_failure_no_unexpected_gives_handled(self):
        # command failed but all errors were known (matched by regex)
        handled = AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)
        c = self._counter()
        c.resolve([False], [handled])
        self.assertEqual(c.run_stats["handled_errors"], 1)

    def test_failure_with_unexpected_gives_failed(self):
        c = self._counter()
        c.resolve([False], [_unexpected_result()])
        self.assertEqual(c.run_stats["failed"], 1)

    def test_single_success_one_command(self):
        c = self._counter()
        c.resolve([True], [_clean_result()])
        self.assertEqual(c.run_stats["clean"], 1)

    def test_mixed_success_unexpected_in_second_gives_failed(self):
        c = self._counter()
        c.resolve([True, False], [_clean_result(), _unexpected_result()])
        self.assertEqual(c.run_stats["failed"], 1)

    def test_multiple_files_counted_independently(self):
        c = self._counter()
        c.resolve([True], [_clean_result()])
        c.resolve([False], [_unexpected_result()])
        self.assertEqual(c.run_stats["clean"], 1)
        self.assertEqual(c.run_stats["failed"], 1)


# ---------------------------------------------------------------------------
# TestsList.run_single
# ---------------------------------------------------------------------------


class TestRunSingle(unittest.TestCase):

    def _make_tests_list(self):
        ignored = MagicMock()
        return TestsList(
            files=[],
            ignored_errors_list=ignored,
            commands=[],
            assets=MagicMock(),
        )

    @patch("tools_run.src.tests_list.run_command")
    @patch("tools_run.src.tests_list.make_command")
    def test_success_returns_true_and_clean_result(self, mock_make, mock_run):
        mock_make.return_value = "tool file.sv"
        mock_run.return_value = _exec_result(ok=True)

        tl = self._make_tests_list()
        cc = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex("error: .*"))
        success, result = tl.run_single("file.sv", "module m; endmodule", cc, "/tmp")

        self.assertTrue(success)
        self.assertEqual(result.unexpected_errors, [])
        self.assertEqual(result.found_matches, [])

    @patch("tools_run.src.tests_list.run_command")
    @patch("tools_run.src.tests_list.make_command")
    def test_failure_no_regex_produces_unexpected_error(self, mock_make, mock_run):
        mock_make.return_value = "tool file.sv"
        mock_run.return_value = _exec_result(ok=False, output="line1\nline2\nline3\nline4")

        tl = self._make_tests_list()
        cc = CommandConfig(run="tool {file}", error_regex=None)
        success, result = tl.run_single("file.sv", "", cc, "/tmp")

        self.assertFalse(success)
        self.assertEqual(len(result.unexpected_errors), 1)
        # excerpt limited to first 3 lines
        self.assertIn("line1", result.unexpected_errors[0].tool_output_error_text)
        self.assertNotIn("line4", result.unexpected_errors[0].tool_output_error_text)

    @patch("tools_run.src.tests_list.run_command")
    @patch("tools_run.src.tests_list.make_command")
    def test_failure_no_regex_empty_output_produces_unexpected_error(self, mock_make, mock_run):
        mock_make.return_value = "tool file.sv"
        mock_run.return_value = _exec_result(ok=False, output="")

        tl = self._make_tests_list()
        cc = CommandConfig(run="tool {file}", error_regex=None)
        success, result = tl.run_single("file.sv", "", cc, "/tmp")

        self.assertFalse(success)
        self.assertEqual(len(result.unexpected_errors), 1)


# ---------------------------------------------------------------------------
# TestsList.run_all — pipeline stop-on-failure
# ---------------------------------------------------------------------------


class TestRunAllPipeline(unittest.TestCase):
    """Tests run_all() sequential pipeline with stop-on-failure logic."""

    def _make_tests_list(self, commands: list[CommandConfig], files):
        ignored = MagicMock()
        ignored.match.return_value = None
        assets = MagicMock()
        assets.copy_to_tmp_dir = MagicMock()
        return TestsList(
            files=files,
            ignored_errors_list=ignored,
            commands=commands,
            assets=assets,
        )

    @patch("tools_run.src.tests_list.run_command")
    @patch("tools_run.src.tests_list.make_command")
    def test_second_command_not_called_when_first_fails(self, mock_make, mock_run):
        mock_make.return_value = "tool file.sv"
        # First call fails, second would succeed — but shouldn't be reached
        mock_run.side_effect = [
            _exec_result(ok=False, output="boom"),
            _exec_result(ok=True),
        ]

        import tempfile
        from pathlib import Path

        with tempfile.NamedTemporaryFile(suffix=".sv", delete=False) as f:
            f.write(b"module m; endmodule")
            path = Path(f.name)

        try:
            cc1 = CommandConfig(run="cmd1 {file}", error_regex=None)
            cc2 = CommandConfig(run="cmd2 {file}", error_regex=None)
            tl = self._make_tests_list([cc1, cc2], [path])
            result = tl.run_all()
        finally:
            path.unlink()

        # Only one run_command call — stopped after first failure
        self.assertEqual(mock_run.call_count, 1)
        self.assertEqual(result.run_stats["failed"], 1)

    @patch("tools_run.src.tests_list.run_command")
    @patch("tools_run.src.tests_list.make_command")
    def test_all_commands_run_when_all_succeed(self, mock_make, mock_run):
        mock_make.return_value = "tool file.sv"
        mock_run.side_effect = [
            _exec_result(ok=True),
            _exec_result(ok=True),
            _exec_result(ok=True),
        ]

        import tempfile
        from pathlib import Path

        with tempfile.NamedTemporaryFile(suffix=".sv", delete=False) as f:
            f.write(b"module m; endmodule")
            path = Path(f.name)

        try:
            commands = [
                CommandConfig(run="cmd1 {file}"),
                CommandConfig(run="cmd2 {file}"),
                CommandConfig(run="cmd3 {file}"),
            ]
            tl = self._make_tests_list(commands, [path])
            result = tl.run_all()
        finally:
            path.unlink()

        self.assertEqual(mock_run.call_count, 3)
        self.assertEqual(result.run_stats["clean"], 1)

    @patch("tools_run.src.tests_list.run_command")
    @patch("tools_run.src.tests_list.make_command")
    def test_single_command_success_is_clean(self, mock_make, mock_run):
        mock_make.return_value = "tool file.sv"
        mock_run.return_value = _exec_result(ok=True)

        import tempfile
        from pathlib import Path

        with tempfile.NamedTemporaryFile(suffix=".sv", delete=False) as f:
            f.write(b"module m; endmodule")
            path = Path(f.name)

        try:
            tl = self._make_tests_list([CommandConfig(run="cmd {file}")], [path])
            result = tl.run_all()
        finally:
            path.unlink()

        self.assertEqual(result.run_stats["clean"], 1)
        self.assertFalse(result.has_unexpected_errors())
