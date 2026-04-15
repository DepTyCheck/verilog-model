"""Tests for common/single_file_runner.py"""

import unittest
from unittest.mock import MagicMock, patch

from common.command_config import CommandConfig
from common.error_types import KnownError, MatchingMode
from common.ignored_errors_list import IgnoredErrorsList
from common.run_command import ExecutionResult
from common.single_file_runner import run_file
from common.tool_error_regex import ToolErrorRegex


def _ok_exec(output: str = "") -> ExecutionResult:
    return ExecutionResult(
        command_executed_successfully=True,
        result_code_is_ok=True,
        timed_out=False,
        output=output,
    )


def _fail_exec(output: str = "boom") -> ExecutionResult:
    return ExecutionResult(
        command_executed_successfully=True,
        result_code_is_ok=False,
        timed_out=False,
        output=output,
    )


def _timeout_exec() -> ExecutionResult:
    return ExecutionResult(
        command_executed_successfully=True,
        result_code_is_ok=False,
        timed_out=True,
        output="timed out",
    )


def _make_ignored(patterns: list[str], mode=MatchingMode.SPECIFIC):
    return IgnoredErrorsList.from_patterns(patterns, mode)


def _run_simple(mock_make, mock_run, exec_result):
    mock_make.return_value = "tool /tmp/x.sv"
    mock_run.return_value = exec_result
    cmd = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex("error: .*"))
    ignored = _make_ignored([])
    return run_file("module m; endmodule", [cmd], ignored, ".sv")


class TestRunFileClean(unittest.TestCase):

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_all_commands_pass_returns_clean(self, mock_make, mock_run):
        result = _run_simple(mock_make, mock_run, _ok_exec())

        self.assertEqual(result.unexpected_errors, [])
        self.assertEqual(result.found_matches, [])
        self.assertTrue(result.all_errors_are_known)

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_timeout_returns_clean(self, mock_make, mock_run):
        result = _run_simple(mock_make, mock_run, _timeout_exec())

        self.assertEqual(result.unexpected_errors, [])
        self.assertTrue(result.all_errors_are_known)


class TestRunFileFailure(unittest.TestCase):

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_unknown_error_returned_as_unexpected(self, mock_make, mock_run):
        mock_make.return_value = "tool /tmp/x.sv"
        mock_run.return_value = _fail_exec("some weird crash")

        cmd = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex("some weird crash"))
        ignored = _make_ignored([])
        result = run_file("module m; endmodule", [cmd], ignored, ".sv")

        self.assertEqual(len(result.unexpected_errors), 1)
        self.assertFalse(result.all_errors_are_known)

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_known_specific_error_returns_found_match(self, mock_make, mock_run):
        mock_make.return_value = "tool /tmp/x.sv"
        mock_run.return_value = _fail_exec("error: syntax error")

        cmd = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex(r"error: .*"))
        ignored = _make_ignored(["error: syntax error"], mode=MatchingMode.SPECIFIC)
        result = run_file("module m; endmodule", [cmd], ignored, ".sv")

        self.assertEqual(result.unexpected_errors, [])
        self.assertEqual(len(result.found_matches), 1)

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_whole_mode_match_no_unexpected_error(self, mock_make, mock_run):
        mock_make.return_value = "tool /tmp/x.sv"
        mock_run.return_value = _fail_exec("Assertion failed in ivl_nexus_s")

        cmd = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex(r"syntax error: .*"))
        ignored = _make_ignored(["Assertion failed"], mode=MatchingMode.WHOLE)
        result = run_file("module m; endmodule", [cmd], ignored, ".sv")

        self.assertEqual(result.unexpected_errors, [])
        self.assertTrue(result.all_errors_are_known)

    @patch("common.single_file_runner.make_command")
    def test_make_command_raises_returns_unexpected(self, mock_make):
        mock_make.side_effect = Exception("No top module found")

        cmd = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex("error: .*"))
        ignored = _make_ignored([])
        result = run_file("module m; endmodule", [cmd], ignored, ".sv")

        self.assertEqual(len(result.unexpected_errors), 1)
        self.assertIn("No top module found", result.unexpected_errors[0].tool_output_error_text)

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_second_command_not_called_when_first_fails(self, mock_make, mock_run):
        mock_make.return_value = "tool /tmp/x.sv"
        mock_run.side_effect = [_fail_exec("boom"), _ok_exec()]

        cmd1 = CommandConfig(run="cmd1 {file}", error_regex=None)
        cmd2 = CommandConfig(run="cmd2 {file}", error_regex=None)
        ignored = _make_ignored([])
        run_file("module m; endmodule", [cmd1, cmd2], ignored, ".sv")

        self.assertEqual(mock_run.call_count, 1)

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_logical_name_used_in_unexpected_error_path(self, mock_make, mock_run):
        mock_make.return_value = "tool /tmp/x.sv"
        mock_run.return_value = _fail_exec("boom")

        cmd = CommandConfig(run="tool {file}", error_regex=None)
        ignored = _make_ignored([])
        result = run_file("content", [cmd], ignored, ".sv", logical_name="/original/path.sv")

        self.assertEqual(result.unexpected_errors[0].test_file_path, "/original/path.sv")


class TestRunFileAssets(unittest.TestCase):

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_assets_copied_to_temp_dir(self, mock_make, mock_run):
        mock_make.return_value = "tool /tmp/x.sv"
        mock_run.return_value = _ok_exec()

        assets = MagicMock()
        cmd = CommandConfig(run="tool {file}")
        ignored = _make_ignored([])
        run_file("content", [cmd], ignored, ".sv", assets=assets)

        assets.copy_to_tmp_dir.assert_called_once()


if __name__ == "__main__":
    unittest.main()
