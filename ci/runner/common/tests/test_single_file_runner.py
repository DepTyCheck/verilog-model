"""Tests for common/single_file_runner.py"""

import unittest
from unittest.mock import MagicMock, patch

from common.command_config import CommandConfig
from common.error_types import MatchingMode
from common.ignored_errors_list import IgnoredErrorsList
from common.run_command import ExecutionResult
from common.run_tool_command import CommandResult
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


def _unknown_results(results: list[CommandResult]) -> list[CommandResult]:
    return [cr for cr in results if cr.outcome == "unknown"]


def _known_records(results: list[CommandResult]):
    return [m for cr in results for m in cr.matches if m.error_id != "unknown"]


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
        results = _run_simple(mock_make, mock_run, _ok_exec())

        self.assertEqual(_unknown_results(results), [])
        self.assertEqual(_known_records(results), [])
        self.assertEqual(len(results), 1)
        self.assertEqual(results[0].outcome, "clean")

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_timeout_returns_timeout_outcome(self, mock_make, mock_run):
        # Behavioural change: timeouts are now an explicit "timeout" outcome with empty
        # matches, not an unknown error. has_unknown_errors() therefore stays False.
        results = _run_simple(mock_make, mock_run, _timeout_exec())

        self.assertEqual(_unknown_results(results), [])
        self.assertEqual(len(results), 1)
        self.assertEqual(results[0].outcome, "timeout")
        self.assertEqual(results[0].matches, [])


class TestRunFileFailure(unittest.TestCase):

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_unknown_error_returned_as_unexpected(self, mock_make, mock_run):
        mock_make.return_value = "tool /tmp/x.sv"
        mock_run.return_value = _fail_exec("some weird crash")

        cmd = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex("some weird crash"))
        ignored = _make_ignored([])
        results = run_file("module m; endmodule", [cmd], ignored, ".sv")

        unknowns = _unknown_results(results)
        self.assertEqual(len(unknowns), 1)
        self.assertEqual(len(unknowns[0].matches), 1)
        self.assertEqual(unknowns[0].matches[0].error_id, "unknown")

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_known_specific_error_returns_found_match(self, mock_make, mock_run):
        mock_make.return_value = "tool /tmp/x.sv"
        mock_run.return_value = _fail_exec("error: syntax error")

        cmd = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex(r"error: .*"))
        ignored = _make_ignored(["error: syntax error"], mode=MatchingMode.SPECIFIC)
        results = run_file("module m; endmodule", [cmd], ignored, ".sv")

        self.assertEqual(_unknown_results(results), [])
        self.assertEqual(len(_known_records(results)), 1)

    @patch("common.single_file_runner.run_command")
    @patch("common.single_file_runner.make_command")
    def test_whole_mode_match_no_unexpected_error(self, mock_make, mock_run):
        mock_make.return_value = "tool /tmp/x.sv"
        mock_run.return_value = _fail_exec("Assertion failed in ivl_nexus_s")

        cmd = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex(r"syntax error: .*"))
        ignored = _make_ignored(["Assertion failed"], mode=MatchingMode.WHOLE)
        results = run_file("module m; endmodule", [cmd], ignored, ".sv")

        self.assertEqual(_unknown_results(results), [])
        self.assertEqual(results[0].outcome, "known_errors")

    @patch("common.single_file_runner.make_command")
    def test_make_command_raises_returns_unexpected(self, mock_make):
        mock_make.side_effect = Exception("No top module found")

        cmd = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex("error: .*"))
        ignored = _make_ignored([])
        results = run_file("module m; endmodule", [cmd], ignored, ".sv")

        unknowns = _unknown_results(results)
        self.assertEqual(len(unknowns), 1)
        self.assertIn("No top module found", unknowns[0].matches[0].matched_text)

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
        # Behavioural change: with no regex, the whole tool output is stored in matched_text.
        # The per-result file path is no longer carried on the match record itself; FileInput
        # logical_name is used by the reporter.
        mock_make.return_value = "tool /tmp/x.sv"
        mock_run.return_value = _fail_exec("boom")

        cmd = CommandConfig(run="tool {file}", error_regex=None)
        ignored = _make_ignored([])
        results = run_file("content", [cmd], ignored, ".sv", logical_name="/original/path.sv")

        unknowns = _unknown_results(results)
        self.assertEqual(len(unknowns), 1)
        self.assertEqual(unknowns[0].matches[0].matched_text, "boom")


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
