# ci/runner/common/tests/test_command_result.py
import unittest

from common.command_config import CommandConfig
from common.error_types import MatchingMode
from common.ignored_errors_list import IgnoredErrorsList
from common.run_command import ExecutionResult
from common.run_tool_command import CommandResult, MatchRecord, analyze_command
from common.tool_error_regex import ToolErrorRegex


class TestCommandResult(unittest.TestCase):
    def test_clean_outcome(self):
        cr = CommandResult(command="iverilog -o /dev/null /tmp/a.sv", outcome="clean", matches=[])
        self.assertEqual(cr.outcome, "clean")
        self.assertEqual(cr.matches, [])

    def test_known_match_record_shape(self):
        m = MatchRecord(error_id="err_x", matched_text="error: x")
        self.assertEqual(m.error_id, "err_x")
        self.assertEqual(m.matched_text, "error: x")

    def test_unknown_match_record_uses_sentinel(self):
        m = MatchRecord(error_id="unknown", matched_text="full output\nline 2")
        self.assertEqual(m.error_id, "unknown")


class TestAnalyzeCommand(unittest.TestCase):
    """Behavioral tests for analyze_command."""

    def _exec(self, output: str = "", ok: bool = False, timed_out: bool = False) -> ExecutionResult:
        return ExecutionResult(
            command_executed_successfully=True,
            result_code_is_ok=ok,
            timed_out=timed_out,
            output=output,
        )

    def test_timeout_outcome(self):
        cmd_cfg = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex("error: .*"))
        result = analyze_command("tool /tmp/x.sv", self._exec(timed_out=True), cmd_cfg, IgnoredErrorsList.from_patterns([]), "/tmp/x.sv")
        self.assertEqual(result.outcome, "timeout")
        self.assertEqual(result.matches, [])

    def test_clean_outcome(self):
        cmd_cfg = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex("error: .*"))
        result = analyze_command("tool /tmp/x.sv", self._exec(ok=True), cmd_cfg, IgnoredErrorsList.from_patterns([]), "/tmp/x.sv")
        self.assertEqual(result.outcome, "clean")
        self.assertEqual(result.matches, [])

    def test_no_regex_records_full_output_as_unknown(self):
        """Per spec: with no regex configured, the whole tool output goes into matched_text."""
        full_output = "line one\nline two\nline three\nline four"
        cmd_cfg = CommandConfig(run="tool {file}", error_regex=None)
        result = analyze_command("tool /tmp/x.sv", self._exec(output=full_output), cmd_cfg, IgnoredErrorsList.from_patterns([]), "/tmp/x.sv")
        self.assertEqual(result.outcome, "unknown")
        self.assertEqual(len(result.matches), 1)
        self.assertEqual(result.matches[0].error_id, "unknown")
        self.assertEqual(result.matches[0].matched_text, full_output)

    def test_known_error_records_match(self):
        cmd_cfg = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex(r"error: .*"))
        ignored = IgnoredErrorsList.from_patterns(["error: syntax error"], MatchingMode.SPECIFIC)
        result = analyze_command(
            "tool /tmp/x.sv",
            self._exec(output="error: syntax error\n"),
            cmd_cfg,
            ignored,
            "/tmp/x.sv",
        )
        self.assertEqual(result.outcome, "known_errors")
        self.assertEqual(len(result.matches), 1)
        self.assertNotEqual(result.matches[0].error_id, "unknown")

    def test_ignored_error_only_classified_as_known_errors(self):
        """Regression test: extra-regex (IgnoredError) passthrough must NOT be reclassified as unknown.

        The tool regex matches; the matched text is then passed through the extra-regex list and
        recognised as an IgnoredError. analyze_command must classify this as known_errors with
        an empty matches list (no per-file report entry, but no unknown either).
        """
        cmd_cfg = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex(r"error: .*"))
        # No KnownError patterns; the extra regex catches the same text.
        ignored = IgnoredErrorsList.from_error_files([], extra_regexes=[r"error: .*"])
        result = analyze_command(
            "tool /tmp/x.sv",
            self._exec(output="error: ignorable thing\n"),
            cmd_cfg,
            ignored,
            "/tmp/x.sv",
        )
        self.assertEqual(result.outcome, "known_errors")
        self.assertEqual(result.matches, [])

    def test_unknown_error_when_regex_matches_nothing(self):
        cmd_cfg = CommandConfig(run="tool {file}", error_regex=ToolErrorRegex(r"never matches: .*"))
        ignored = IgnoredErrorsList.from_patterns([], MatchingMode.SPECIFIC)
        result = analyze_command(
            "tool /tmp/x.sv",
            self._exec(output="some other failure\n"),
            cmd_cfg,
            ignored,
            "/tmp/x.sv",
        )
        self.assertEqual(result.outcome, "unknown")
        self.assertEqual(len(result.matches), 1)
        self.assertEqual(result.matches[0].error_id, "unknown")


if __name__ == "__main__":
    unittest.main()
