"""
Unit tests for regression_test/src/error_checker.py.

All tests are pure-Python: no real tool execution happens.
run_command / make_command are patched so we can control tool output precisely.
"""

import os
import tempfile
import textwrap
import unittest
from unittest.mock import patch

from common.command_config import CommandConfig
from common.error_file_parser import Example
from common.error_types import KnownError, MatchingMode
from common.run_command import ExecutionResult
from common.tool_error_regex import ToolErrorRegex
from regression_test.src.error_checker import ToolConfig, _load_all_error_files, _run_example, check_all
from tools_run.src.ignored_errors_list import IgnoredErrorsList


def _make_tool(name: str = "fake_tool", regex: str | None = "known error") -> ToolConfig:
    cmd = CommandConfig(run="fake {file}", error_regex=ToolErrorRegex(regex) if regex else None)
    return ToolConfig(name=name, commands=[cmd])


def _make_ignored_errors(patterns: list[str]) -> IgnoredErrorsList:
    instance = IgnoredErrorsList.__new__(IgnoredErrorsList)
    instance._tool = None
    instance._errors = [KnownError(error_id=f"err_{i}", pattern=p, mode=MatchingMode.SPECIFIC) for i, p in enumerate(patterns)]
    instance._extra_regexes = []
    return instance


def _fake_result(output: str, ok: bool = True, timed_out: bool = False) -> ExecutionResult:
    return ExecutionResult(
        command_executed_successfully=True,
        result_code_is_ok=ok,
        timed_out=timed_out,
        output=output,
    )


# ---------------------------------------------------------------------------
# _run_example tests
# ---------------------------------------------------------------------------


class TestRunExample(unittest.TestCase):

    def _make_example(self, content: str = "module m(); endmodule") -> Example:
        return Example(name="ex1", type="minified", content=content)

    @patch("regression_test.src.error_checker.run_command")
    @patch("regression_test.src.error_checker.make_command")
    def test_all_commands_pass_returns_clean(self, mock_make, mock_run):
        mock_make.return_value = "fake file.sv"
        mock_run.return_value = _fake_result("", ok=True)

        tool = _make_tool()
        ignored = _make_ignored_errors(["known error"])
        result = _run_example(self._make_example(), tool.commands, ignored, "sv")

        self.assertEqual(result.unexpected_errors, [])
        self.assertEqual(result.found_matches, [])
        self.assertTrue(result.all_errors_are_known)

    @patch("regression_test.src.error_checker.run_command")
    @patch("regression_test.src.error_checker.make_command")
    def test_known_error_in_output_returns_found_match(self, mock_make, mock_run):
        mock_make.return_value = "fake file.sv"
        mock_run.return_value = _fake_result("known error found here", ok=False)

        tool = _make_tool(regex="known error")
        ignored = _make_ignored_errors(["known error"])
        result = _run_example(self._make_example(), tool.commands, ignored, "sv")

        self.assertEqual(result.unexpected_errors, [])
        self.assertEqual(len(result.found_matches), 1)

    @patch("regression_test.src.error_checker.run_command")
    @patch("regression_test.src.error_checker.make_command")
    def test_unknown_error_in_output_returns_unexpected(self, mock_make, mock_run):
        mock_make.return_value = "fake file.sv"
        mock_run.return_value = _fake_result("some unexpected failure", ok=False)

        tool = _make_tool(regex="some unexpected failure")
        ignored = _make_ignored_errors([])  # nothing is known
        result = _run_example(self._make_example(), tool.commands, ignored, "sv")

        self.assertEqual(len(result.unexpected_errors), 1)
        self.assertFalse(result.all_errors_are_known)

    @patch("regression_test.src.error_checker.run_command")
    @patch("regression_test.src.error_checker.make_command")
    def test_timeout_returns_no_unexpected_errors(self, mock_make, mock_run):
        mock_make.return_value = "fake file.sv"
        mock_run.return_value = _fake_result("timed out", ok=False, timed_out=True)

        tool = _make_tool()
        ignored = _make_ignored_errors([])
        result = _run_example(self._make_example(), tool.commands, ignored, "sv")

        self.assertEqual(result.unexpected_errors, [])

    @patch("regression_test.src.error_checker.make_command")
    def test_make_command_raises_returns_unexpected(self, mock_make):
        mock_make.side_effect = Exception("No top module found")

        tool = _make_tool()
        ignored = _make_ignored_errors([])
        result = _run_example(self._make_example(), tool.commands, ignored, "sv")

        self.assertEqual(len(result.unexpected_errors), 1)
        self.assertIn("No top module found", result.unexpected_errors[0].tool_output_error_text)

    @patch("regression_test.src.error_checker.run_command")
    @patch("regression_test.src.error_checker.make_command")
    def test_stops_on_first_failing_command(self, mock_make, mock_run):
        """With two commands, only the first (failing) one should be run."""
        mock_make.return_value = "fake file.sv"
        mock_run.return_value = _fake_result("known error found", ok=False)

        cmd1 = CommandConfig(run="cmd1 {file}", error_regex=ToolErrorRegex("known error"))
        cmd2 = CommandConfig(run="cmd2 {file}", error_regex=ToolErrorRegex("other error"))
        tool = ToolConfig(name="fake_tool", commands=[cmd1, cmd2])
        ignored = _make_ignored_errors(["known error"])

        _run_example(self._make_example(), tool.commands, ignored, "sv")

        self.assertEqual(mock_run.call_count, 1)


# ---------------------------------------------------------------------------
# Helpers for check_all / _load_all_error_files tests
# ---------------------------------------------------------------------------

_TOOL_A_YAML = textwrap.dedent("""\
    id: error_a
    tool: tool-a
    regex: 'known error from a'
    examples:
      - ex1:
          minified_example: "module bad(); endmodule"
""")

_TOOL_B_YAML = textwrap.dedent("""\
    id: error_b
    tool: tool-b
    regex: 'known error from b'
    examples:
      - ex1:
          minified_example: "entity bad is end bad;"
""")

_TWO_EXAMPLES_WITH_FULL_YAML = textwrap.dedent("""\
    id: two_ex_error
    tool: tool-a
    regex: 'known error from a'
    examples:
      - example_1:
          minified_example: "module bad1(); endmodule"
          full_example: "// full\\nmodule bad1_full(output o); assign o = ; endmodule"
      - example_2:
          minified_example: "module bad2(); endmodule"
          full_example: "// full\\nmodule bad2_full(output o); assign o = ; endmodule"
""")


def _write_known_errors_dir(tmp: str, files: dict[str, str]) -> str:
    for rel_path, content in files.items():
        full_path = os.path.join(tmp, rel_path)
        os.makedirs(os.path.dirname(full_path), exist_ok=True)
        with open(full_path, "w") as f:
            f.write(content)
    return tmp


class TestLoadAllErrorFiles(unittest.TestCase):

    def test_loads_from_all_subdirs(self):
        with tempfile.TemporaryDirectory() as tmp:
            _write_known_errors_dir(tmp, {"tool-a/error_a.yaml": _TOOL_A_YAML, "tool-b/error_b.yaml": _TOOL_B_YAML})
            files = _load_all_error_files(tmp)
        self.assertEqual({f.error_id for f in files}, {"error_a", "error_b"})

    def test_returns_empty_for_missing_dir(self):
        self.assertEqual(_load_all_error_files("/nonexistent/path"), [])

    def test_originating_tool_set_from_yaml(self):
        with tempfile.TemporaryDirectory() as tmp:
            _write_known_errors_dir(tmp, {"tool-a/error_a.yaml": _TOOL_A_YAML})
            files = _load_all_error_files(tmp)
        self.assertEqual(files[0].tool, "tool-a")


# ---------------------------------------------------------------------------
# check_all tests
# ---------------------------------------------------------------------------


class TestCheckAll(unittest.TestCase):

    def _make_tool(self, name: str, regex: str | None = "known error") -> ToolConfig:
        cmd = CommandConfig(run="fake {file}", error_regex=ToolErrorRegex(regex) if regex else None)
        return ToolConfig(name=name, commands=[cmd])

    @patch("regression_test.src.error_checker.run_command")
    @patch("regression_test.src.error_checker.make_command")
    def test_unknown_error_from_other_tool_is_flagged(self, mock_make, mock_run):
        """tool-a running against tool-b's example produces an unknown error → CI fails."""
        mock_make.return_value = "fake file.sv"
        mock_run.return_value = _fake_result("totally unexpected failure", ok=False)

        with tempfile.TemporaryDirectory() as tmp:
            _write_known_errors_dir(tmp, {"tool-b/error_b.yaml": _TOOL_B_YAML})
            tool = self._make_tool("tool-a", regex="totally unexpected failure")
            _, new_errors, regressions = check_all(tmp, tool)

        self.assertEqual(len(new_errors), 1)
        self.assertEqual(new_errors[0]["originating_tool"], "tool-b")
        self.assertEqual(regressions, [])

    @patch("regression_test.src.error_checker.run_command")
    @patch("regression_test.src.error_checker.make_command")
    def test_own_error_reproduces(self, mock_make, mock_run):
        """tool-a's own error still in output → reproduced=True."""
        mock_make.return_value = "fake file.sv"
        mock_run.return_value = _fake_result("known error from a", ok=False)

        with tempfile.TemporaryDirectory() as tmp:
            _write_known_errors_dir(tmp, {"tool-a/error_a.yaml": _TOOL_A_YAML})
            tool = self._make_tool("tool-a", regex="known error from a")
            _, new_errors, regressions = check_all(tmp, tool)

        self.assertEqual(new_errors, [])
        self.assertEqual(len(regressions), 1)
        self.assertTrue(regressions[0]["reproduced"])

    @patch("regression_test.src.error_checker.run_command")
    @patch("regression_test.src.error_checker.make_command")
    def test_own_error_not_reproducing(self, mock_make, mock_run):
        """tool-a's own error no longer in output → reproduced=False, CI still passes."""
        mock_make.return_value = "fake file.sv"
        mock_run.return_value = _fake_result("", ok=True)

        with tempfile.TemporaryDirectory() as tmp:
            _write_known_errors_dir(tmp, {"tool-a/error_a.yaml": _TOOL_A_YAML})
            tool = self._make_tool("tool-a")
            _, new_errors, regressions = check_all(tmp, tool)

        self.assertEqual(new_errors, [])
        self.assertEqual(len(regressions), 1)
        self.assertFalse(regressions[0]["reproduced"])

    @patch("regression_test.src.error_checker.run_command")
    @patch("regression_test.src.error_checker.make_command")
    def test_cross_tool_scan_covers_all_subdirs(self, mock_make, mock_run):
        """tool-a is run against both tool-a and tool-b errors; only tool-a in regressions."""
        mock_make.return_value = "fake file.sv"
        mock_run.return_value = _fake_result("", ok=True)

        with tempfile.TemporaryDirectory() as tmp:
            _write_known_errors_dir(tmp, {"tool-a/error_a.yaml": _TOOL_A_YAML, "tool-b/error_b.yaml": _TOOL_B_YAML})
            tool = self._make_tool("tool-a")
            error_results, new_errors, regressions = check_all(tmp, tool)

        self.assertEqual(new_errors, [])
        self.assertEqual(len(regressions), 1)
        self.assertEqual(regressions[0]["error_id"], "error_a")
        self.assertEqual(len(error_results), 1)
        self.assertEqual(error_results[0].originating_tool, "tool-a")

    def test_empty_dir_returns_empty(self):
        with tempfile.TemporaryDirectory() as tmp:
            tool = self._make_tool("tool-a")
            error_results, new_errors, regressions = check_all(tmp, tool)
        self.assertEqual((error_results, new_errors, regressions), ([], [], []))

    @patch("regression_test.src.error_checker.run_command")
    @patch("regression_test.src.error_checker.make_command")
    def test_two_examples_with_minified_and_full_all_reproduce(self, mock_make, mock_run):
        """Two examples each with minified+full, all reproducing → 4 regression marks."""
        mock_make.return_value = "fake file.sv"
        mock_run.return_value = _fake_result("known error from a", ok=False)

        with tempfile.TemporaryDirectory() as tmp:
            _write_known_errors_dir(tmp, {"tool-a/two_examples.yaml": _TWO_EXAMPLES_WITH_FULL_YAML})
            tool = self._make_tool("tool-a", regex="known error from a")
            _, new_errors, regressions = check_all(tmp, tool)

        self.assertEqual(new_errors, [])
        self.assertEqual(len(regressions), 4)
        for reg in regressions:
            self.assertTrue(reg["reproduced"])

        example_types = {(r["example_name"], r["example_type"]) for r in regressions}
        self.assertEqual(
            example_types,
            {("example_1", "minified"), ("example_1", "full"), ("example_2", "minified"), ("example_2", "full")},
        )
