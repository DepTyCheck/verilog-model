"""Tests for common/single_file_runner.py"""

import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

from common.command_config import CommandConfig
from common.command_output import AnalyzisResult
from common.error_types import KnownError, MatchingMode, UnexpectedError
from common.run_command import ExecutionResult
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
    from tools_run.src.ignored_errors_list import IgnoredErrorsList

    instance = IgnoredErrorsList.__new__(IgnoredErrorsList)
    instance._tool = None
    instance._errors = [KnownError(error_id=f"e{i}", pattern=p, mode=mode) for i, p in enumerate(patterns)]
    instance._extra_regexes = []
    return instance


class TestRunFileClean(unittest.TestCase):

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_all_commands_pass_returns_clean(self, mock_make, mock_run):
        from common.single_file_runner import run_file

        mock_make.return_value = "tool /tmp/x.sv"
        mock_run.return_value = _ok_exec()

        cmd = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex("error: .*"))
        ignored = _make_ignored([])
        result = run_file("module m; endmodule", [cmd], ignored, ".sv")

        self.assertEqual(result.unexpected_errors, [])
        self.assertEqual(result.found_matches, [])
        self.assertTrue(result.all_errors_are_known)

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_timeout_returns_clean(self, mock_make, mock_run):
        from common.single_file_runner import run_file

        mock_make.return_value = "tool /tmp/x.sv"
        mock_run.return_value = _timeout_exec()

        cmd = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex("error: .*"))
        ignored = _make_ignored([])
        result = run_file("module m; endmodule", [cmd], ignored, ".sv")

        self.assertEqual(result.unexpected_errors, [])
        self.assertTrue(result.all_errors_are_known)


class TestRunFileFailure(unittest.TestCase):

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_unknown_error_returned_as_unexpected(self, mock_make, mock_run):
        from common.single_file_runner import run_file

        mock_make.return_value = "tool /tmp/x.sv"
        mock_run.return_value = _fail_exec("some weird crash")

        cmd = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex("some weird crash"))
        ignored = _make_ignored([])  # nothing known
        result = run_file("module m; endmodule", [cmd], ignored, ".sv")

        self.assertEqual(len(result.unexpected_errors), 1)
        self.assertFalse(result.all_errors_are_known)

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_known_specific_error_returns_found_match(self, mock_make, mock_run):
        from common.single_file_runner import run_file

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
        from common.single_file_runner import run_file

        mock_make.return_value = "tool /tmp/x.sv"
        # Output that does NOT match tool error regex but DOES match WHOLE pattern
        mock_run.return_value = _fail_exec("Assertion failed in ivl_nexus_s")

        cmd = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex(r"syntax error: .*"))
        ignored = _make_ignored(["Assertion failed"], mode=MatchingMode.WHOLE)
        result = run_file("module m; endmodule", [cmd], ignored, ".sv")

        # WHOLE mode absorbed the error — no unexpected errors
        self.assertEqual(result.unexpected_errors, [])
        self.assertTrue(result.all_errors_are_known)

    @patch("common.single_file_runner.make_command")
    def test_make_command_raises_returns_unexpected(self, mock_make):
        from common.single_file_runner import run_file

        mock_make.side_effect = Exception("No top module found")

        cmd = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex("error: .*"))
        ignored = _make_ignored([])
        result = run_file("module m; endmodule", [cmd], ignored, ".sv")

        self.assertEqual(len(result.unexpected_errors), 1)
        self.assertIn("No top module found", result.unexpected_errors[0].tool_output_error_text)

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_second_command_not_called_when_first_fails(self, mock_make, mock_run):
        from common.single_file_runner import run_file

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
        from common.single_file_runner import run_file

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
        from common.single_file_runner import run_file

        mock_make.return_value = "tool /tmp/x.sv"
        mock_run.return_value = _ok_exec()

        assets = MagicMock()
        cmd = CommandConfig(run="tool {file}")
        ignored = _make_ignored([])
        run_file("content", [cmd], ignored, ".sv", assets=assets)

        assets.copy_to_tmp_dir.assert_called_once()


if __name__ == "__main__":
    import unittest

    unittest.main()
