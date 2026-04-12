"""
Unit tests for known_errors_check/src/error_checker.py.

All tests are pure-Python: no real tool execution happens.
The _classify() and _run_example_with_tool() functions are exercised through
mocking run_command() / make_command() so we can control tool output precisely.
check_all cross-tool behaviour is tested by building a temporary directory tree
with minimal YAML error files and patching run_command.
"""

import os
import tempfile
import textwrap
import unittest
from unittest.mock import patch

from common.error_file_parser import ErrorFile, Example
from common.error_types import MatchingMode
from common.run_command import ExecutionResult
from common.tool_error_regex import ToolErrorRegex
from known_errors_check.src.error_checker import ExampleStatus, ToolConfig, _classify, _load_all_error_files, _run_example_with_tool, check_all


def _make_error_file(regex: str = "known error pattern", mode: MatchingMode = MatchingMode.SPECIFIC) -> ErrorFile:
    return ErrorFile(
        error_id="test_error",
        tool="fake_tool",
        regex=regex,
        mode=mode,
        title="Test error",
        examples=[],
    )


def _make_example(content: str = "module m(); endmodule") -> Example:
    return Example(name="ex1", type="minified", content=content)


def _make_tool(cmd: str = "echo ''", regex: str | None = "known error pattern") -> ToolConfig:
    return ToolConfig(name="fake_tool", cmd=cmd, error_regex=ToolErrorRegex(regex) if regex else None)


class TestClassify(unittest.TestCase):

    def test_timeout_returns_timeout_status(self):
        ef = _make_error_file()
        status, _ = _classify("", result_code_ok=False, timed_out=True, error_file=ef, tool_regex=ToolErrorRegex(ef.regex))
        self.assertEqual(status, ExampleStatus.TIMEOUT)

    def test_result_ok_no_errors_returns_clean(self):
        ef = _make_error_file()
        status, excerpt = _classify("all good", result_code_ok=True, timed_out=False, error_file=ef, tool_regex=ToolErrorRegex("known error pattern"))
        self.assertEqual(status, ExampleStatus.CLEAN)
        self.assertEqual(excerpt, "")

    def test_known_error_specific_match(self):
        ef = _make_error_file(regex="known error pattern")
        output = "blah known error pattern blah"
        tool_regex = ToolErrorRegex("known error pattern")
        status, excerpt = _classify(output, result_code_ok=False, timed_out=False, error_file=ef, tool_regex=tool_regex)
        self.assertEqual(status, ExampleStatus.KNOWN_ERROR)
        self.assertIn("known error pattern", excerpt)

    def test_new_error_when_extracted_error_does_not_match_known_regex(self):
        ef = _make_error_file(regex="known error pattern")
        output = "completely different unexpected failure"
        tool_regex = ToolErrorRegex(r"completely different unexpected failure")
        status, excerpt = _classify(output, result_code_ok=False, timed_out=False, error_file=ef, tool_regex=tool_regex)
        self.assertEqual(status, ExampleStatus.NEW_ERROR)

    def test_new_error_non_zero_exit_no_match(self):
        ef = _make_error_file(regex="known error pattern")
        output = "some unrelated output"
        tool_regex = ToolErrorRegex("this regex matches nothing")
        status, _ = _classify(output, result_code_ok=False, timed_out=False, error_file=ef, tool_regex=tool_regex)
        self.assertEqual(status, ExampleStatus.NEW_ERROR)

    def test_whole_mode_known_error(self):
        ef = _make_error_file(regex="whole output match", mode=MatchingMode.WHOLE)
        output = "something something whole output match something"
        tool_regex = ToolErrorRegex("should not extract anything specific")
        status, _ = _classify(output, result_code_ok=False, timed_out=False, error_file=ef, tool_regex=tool_regex)
        self.assertEqual(status, ExampleStatus.KNOWN_ERROR)

    def test_whole_mode_new_error_when_no_match(self):
        ef = _make_error_file(regex="expected pattern not present", mode=MatchingMode.WHOLE)
        output = "completely different failure"
        tool_regex = ToolErrorRegex("no match")
        status, _ = _classify(output, result_code_ok=False, timed_out=False, error_file=ef, tool_regex=tool_regex)
        self.assertEqual(status, ExampleStatus.NEW_ERROR)

    def test_specific_fallback_to_whole_when_tool_regex_matches_nothing(self):
        # tool_regex finds nothing in SPECIFIC mode → fall through to whole match
        ef = _make_error_file(regex="whole fallback pattern", mode=MatchingMode.SPECIFIC)
        output = "whole fallback pattern present here"
        tool_regex = ToolErrorRegex("this regex matches nothing in output")
        status, _ = _classify(output, result_code_ok=False, timed_out=False, error_file=ef, tool_regex=tool_regex)
        self.assertEqual(status, ExampleStatus.KNOWN_ERROR)

    def test_none_tool_regex_known_error_via_whole_match(self):
        # tool without error_regex: falls straight to whole-output match
        ef = _make_error_file(regex="known error pattern")
        output = "something known error pattern something"
        status, _ = _classify(output, result_code_ok=False, timed_out=False, error_file=ef, tool_regex=None)
        self.assertEqual(status, ExampleStatus.KNOWN_ERROR)

    def test_none_tool_regex_new_error_when_no_whole_match(self):
        ef = _make_error_file(regex="expected pattern not here")
        output = "completely different failure"
        status, _ = _classify(output, result_code_ok=False, timed_out=False, error_file=ef, tool_regex=None)
        self.assertEqual(status, ExampleStatus.NEW_ERROR)


class TestRunExampleWithTool(unittest.TestCase):

    def _fake_execution_result(self, output: str, ok: bool = True, timed_out: bool = False) -> ExecutionResult:
        return ExecutionResult(
            command_executed_successfully=True,
            result_code_is_ok=ok,
            timed_out=timed_out,
            output=output,
        )

    @patch("known_errors_check.src.error_checker.run_command")
    @patch("known_errors_check.src.error_checker.make_command")
    def test_clean_when_tool_passes(self, mock_make, mock_run):
        mock_make.return_value = "fake_cmd file.sv"
        mock_run.return_value = self._fake_execution_result("", ok=True)

        ef = _make_error_file()
        example = _make_example()
        tool = _make_tool()

        result = _run_example_with_tool(example, ef, tool)
        self.assertEqual(result.status, ExampleStatus.CLEAN)

    @patch("known_errors_check.src.error_checker.run_command")
    @patch("known_errors_check.src.error_checker.make_command")
    def test_known_error_when_output_matches(self, mock_make, mock_run):
        mock_make.return_value = "fake_cmd file.sv"
        mock_run.return_value = self._fake_execution_result("known error pattern found", ok=False)

        ef = _make_error_file(regex="known error pattern")
        example = _make_example()
        tool = _make_tool(regex="known error pattern")

        result = _run_example_with_tool(example, ef, tool)
        self.assertEqual(result.status, ExampleStatus.KNOWN_ERROR)

    @patch("known_errors_check.src.error_checker.run_command")
    @patch("known_errors_check.src.error_checker.make_command")
    def test_new_error_when_unexpected_output(self, mock_make, mock_run):
        mock_make.return_value = "fake_cmd file.sv"
        mock_run.return_value = self._fake_execution_result("unexpected failure message", ok=False)

        ef = _make_error_file(regex="known error pattern")
        example = _make_example()
        tool = _make_tool(regex="unexpected failure message")

        result = _run_example_with_tool(example, ef, tool)
        self.assertEqual(result.status, ExampleStatus.NEW_ERROR)

    @patch("known_errors_check.src.error_checker.run_command")
    @patch("known_errors_check.src.error_checker.make_command")
    def test_timeout(self, mock_make, mock_run):
        mock_make.return_value = "fake_cmd file.sv"
        mock_run.return_value = self._fake_execution_result("timed out", ok=False, timed_out=True)

        ef = _make_error_file()
        example = _make_example()
        tool = _make_tool()

        result = _run_example_with_tool(example, ef, tool)
        self.assertEqual(result.status, ExampleStatus.TIMEOUT)

    @patch("known_errors_check.src.error_checker.make_command")
    def test_exec_error_when_make_command_raises(self, mock_make):
        mock_make.side_effect = Exception("No top module found")

        ef = _make_error_file()
        example = _make_example()
        tool = _make_tool()

        result = _run_example_with_tool(example, ef, tool)
        self.assertEqual(result.status, ExampleStatus.EXEC_ERROR)
        self.assertIn("No top module found", result.output_excerpt)


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

_TOOL_B_YAML = textwrap.dedent("""\
    id: error_b
    tool: tool-b
    regex: 'known error from b'
    examples:
      - ex1:
          minified_example: "entity bad is end bad;"
""")


def _write_known_errors_dir(tmp: str, files: dict[str, str]) -> str:
    """
    Write YAML files into a known_errors directory tree.

    files maps "subdir/filename.yaml" → yaml content.
    Returns the path to the root directory.
    """
    for rel_path, content in files.items():
        full_path = os.path.join(tmp, rel_path)
        os.makedirs(os.path.dirname(full_path), exist_ok=True)
        with open(full_path, "w") as f:
            f.write(content)
    return tmp


def _fake_result(output: str, ok: bool = True, timed_out: bool = False) -> ExecutionResult:
    return ExecutionResult(
        command_executed_successfully=True,
        result_code_is_ok=ok,
        timed_out=timed_out,
        output=output,
    )


class TestLoadAllErrorFiles(unittest.TestCase):

    def test_loads_from_all_subdirs(self):
        with tempfile.TemporaryDirectory() as tmp:
            _write_known_errors_dir(
                tmp,
                {
                    "tool-a/error_a.yaml": _TOOL_A_YAML,
                    "tool-b/error_b.yaml": _TOOL_B_YAML,
                },
            )
            files = _load_all_error_files(tmp)
        ids = {f.error_id for f in files}
        self.assertEqual(ids, {"error_a", "error_b"})

    def test_returns_empty_for_missing_dir(self):
        self.assertEqual(_load_all_error_files("/nonexistent/path"), [])

    def test_originating_tool_set_from_yaml(self):
        with tempfile.TemporaryDirectory() as tmp:
            _write_known_errors_dir(tmp, {"tool-a/error_a.yaml": _TOOL_A_YAML})
            files = _load_all_error_files(tmp)
        self.assertEqual(files[0].tool, "tool-a")


class TestCheckAll(unittest.TestCase):

    def _make_tool(self, name: str, regex: str | None = "known error") -> ToolConfig:
        return ToolConfig(name=name, cmd="fake {file}", error_regex=ToolErrorRegex(regex) if regex else None)

    @patch("known_errors_check.src.error_checker.run_command")
    @patch("known_errors_check.src.error_checker.make_command")
    def test_new_error_from_other_tool_errors_dir_is_flagged(self, mock_make, mock_run):
        """Tool-A running against Tool-B's example and producing a NEW_ERROR fails CI."""
        mock_make.return_value = "fake file.sv"
        mock_run.return_value = _fake_result("totally unexpected failure", ok=False)

        with tempfile.TemporaryDirectory() as tmp:
            _write_known_errors_dir(tmp, {"tool-b/error_b.yaml": _TOOL_B_YAML})
            tool = self._make_tool("tool-a", regex="known error from a")
            _, new_errors, regressions = check_all(tmp, tool)

        self.assertEqual(len(new_errors), 1)
        self.assertEqual(new_errors[0]["originating_tool"], "tool-b")
        self.assertEqual(new_errors[0]["checked_with_tool"], "tool-a")
        self.assertEqual(regressions, [])  # tool-a has no own errors in this dir

    @patch("known_errors_check.src.error_checker.run_command")
    @patch("known_errors_check.src.error_checker.make_command")
    def test_own_known_error_adds_to_regression_confirmations(self, mock_make, mock_run):
        """Tool-A's own error still reproducing → in regression_confirmations."""
        mock_make.return_value = "fake file.sv"
        mock_run.return_value = _fake_result("known error from a", ok=False)

        with tempfile.TemporaryDirectory() as tmp:
            _write_known_errors_dir(tmp, {"tool-a/error_a.yaml": _TOOL_A_YAML})
            tool = self._make_tool("tool-a", regex="known error from a")
            _, new_errors, regressions = check_all(tmp, tool)

        self.assertEqual(new_errors, [])
        self.assertEqual(len(regressions), 1)
        self.assertEqual(regressions[0]["status"], ExampleStatus.KNOWN_ERROR.value)
        self.assertEqual(regressions[0]["originating_tool"], "tool-a")

    @patch("known_errors_check.src.error_checker.run_command")
    @patch("known_errors_check.src.error_checker.make_command")
    def test_own_error_clean_appears_in_regressions_not_new_errors(self, mock_make, mock_run):
        """A 'fixed' bug (CLEAN result) goes to regressions, not new_errors."""
        mock_make.return_value = "fake file.sv"
        mock_run.return_value = _fake_result("", ok=True)

        with tempfile.TemporaryDirectory() as tmp:
            _write_known_errors_dir(tmp, {"tool-a/error_a.yaml": _TOOL_A_YAML})
            tool = self._make_tool("tool-a")
            _, new_errors, regressions = check_all(tmp, tool)

        self.assertEqual(new_errors, [])
        self.assertEqual(len(regressions), 1)
        self.assertEqual(regressions[0]["status"], ExampleStatus.CLEAN.value)

    @patch("known_errors_check.src.error_checker.run_command")
    @patch("known_errors_check.src.error_checker.make_command")
    def test_cross_tool_scan_covers_all_subdirs(self, mock_make, mock_run):
        """Tool-A is run against both tool-a and tool-b known errors."""
        mock_make.return_value = "fake file.sv"
        mock_run.return_value = _fake_result("", ok=True)  # CLEAN for everything

        with tempfile.TemporaryDirectory() as tmp:
            _write_known_errors_dir(
                tmp,
                {
                    "tool-a/error_a.yaml": _TOOL_A_YAML,
                    "tool-b/error_b.yaml": _TOOL_B_YAML,
                },
            )
            tool = self._make_tool("tool-a")
            error_results, new_errors, regressions = check_all(tmp, tool)

        # Both error files are covered
        self.assertEqual(len(error_results), 2)
        self.assertEqual(new_errors, [])
        # Only tool-a's own error appears in regressions
        self.assertEqual(len(regressions), 1)
        self.assertEqual(regressions[0]["originating_tool"], "tool-a")

    def test_empty_known_errors_dir_returns_empty(self):
        with tempfile.TemporaryDirectory() as tmp:
            tool = self._make_tool("tool-a")
            error_results, new_errors, regressions = check_all(tmp, tool)
        self.assertEqual(error_results, [])
        self.assertEqual(new_errors, [])
        self.assertEqual(regressions, [])

    @patch("known_errors_check.src.error_checker.run_command")
    @patch("known_errors_check.src.error_checker.make_command")
    def test_two_examples_with_minified_and_full_all_reproduce(self, mock_make, mock_run):
        """Two examples each with minified+full, all reproducing → 4 regression marks (2 examples × 2 types)."""
        mock_make.return_value = "fake file.sv"
        mock_run.return_value = _fake_result("known error from a", ok=False)

        with tempfile.TemporaryDirectory() as tmp:
            _write_known_errors_dir(tmp, {"tool-a/two_examples.yaml": _TWO_EXAMPLES_WITH_FULL_YAML})
            tool = self._make_tool("tool-a", regex="known error from a")
            error_results, new_errors, regressions = check_all(tmp, tool)

        self.assertEqual(new_errors, [])

        # All 4 runs (example_1/minified, example_1/full, example_2/minified, example_2/full) are tracked
        self.assertEqual(len(regressions), 4)
        # Every run matches the known error → all 4 marks are KNOWN_ERROR
        for reg in regressions:
            self.assertEqual(reg["status"], ExampleStatus.KNOWN_ERROR.value)

        # example_types covered: both minified and full are present
        example_types = {(r["example_name"], r["example_type"]) for r in regressions}
        self.assertEqual(
            example_types,
            {("example_1", "minified"), ("example_1", "full"), ("example_2", "minified"), ("example_2", "full")},
        )
