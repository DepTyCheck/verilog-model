# ci/runner/common/tests/test_command_result.py
import unittest

from common.command_config import CommandConfig
from common.error_file_parser import ErrorFile
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

    def test_location_grouping_carves_invalid_module_item(self):
        output = (
            "tmp/x,y.sv:41: syntax error\n"
            "tmp/x,y.sv:41: error: Syntax error in instance port expression(s).\n"
            "tmp/x,y.sv:41: error: Invalid module item.\n"
        )
        cmd_cfg = CommandConfig(
            run="tool {file}",
            error_regex=ToolErrorRegex(r"^[A-z0-9_.\/,-]+:\d+: .+$"),
        )
        ignored = IgnoredErrorsList.from_error_files(
            [
                ErrorFile(
                    error_id="e0",
                    tool="tool",
                    regex=r"Syntax error in instance port expression",
                    mode=MatchingMode.SPECIFIC,
                    title="",
                    profile="sv",
                ),
                ErrorFile(
                    error_id="e1",
                    tool="tool",
                    regex=r"Invalid module item\.",
                    mode=MatchingMode.SPECIFIC,
                    title="",
                    profile="sv",
                ),
            ],
            extra_regexes=[r"syntax error$"],
        )
        result = analyze_command(
            "tool /tmp/x.sv",
            self._exec(output=output),
            cmd_cfg,
            ignored,
            "/tmp/x.sv",
            location_regex=r"^[A-z0-9_.\/,-]+:(\d+):",
        )
        self.assertEqual(result.outcome, "known_errors")
        ids = sorted(m.error_id for m in result.matches)
        self.assertEqual(ids, ["e0", "e1"])

    def test_location_grouping_leftover_becomes_unknown(self):
        output = (
            "tmp/x.sv:10: known error here\n"
            "tmp/x.sv:20: completely unrecognised error\n"
        )
        cmd_cfg = CommandConfig(
            run="tool {file}",
            error_regex=ToolErrorRegex(r"^[A-z0-9_.\/,-]+:\d+: .+$"),
        )
        ignored = IgnoredErrorsList.from_patterns(
            [r"known error here"],
            MatchingMode.SPECIFIC,
        )
        result = analyze_command(
            "tool /tmp/x.sv",
            self._exec(output=output),
            cmd_cfg,
            ignored,
            "/tmp/x.sv",
            location_regex=r"^[A-z0-9_.\/,-]+:(\d+):",
        )
        self.assertEqual(result.outcome, "unknown")
        unknown_ids = [m.error_id for m in result.matches if m.error_id == "unknown"]
        self.assertGreater(len(unknown_ids), 0)

    def test_without_location_regex_legacy_path_unchanged(self):
        """Without location_regex, the legacy per-atom path must still work."""
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

    def test_grouping_extra_and_known_same_line_yields_only_known_id(self):
        """Same-line group: extra_ignored matches one atom, KnownError matches another.

        Expected: outcome=known_errors, matches contains only the KnownError id,
        no 'unknown', and the extra/ignored atom does NOT surface as an error_id.
        """
        output = (
            "tmp/x.sv:10: syntax error\n"
            "tmp/x.sv:10: error: Invalid module item.\n"
        )
        cmd_cfg = CommandConfig(
            run="tool {file}",
            error_regex=ToolErrorRegex(r"^[A-z0-9_.\/,-]+:\d+: .+$"),
        )
        ignored = IgnoredErrorsList.from_error_files(
            [
                ErrorFile(
                    error_id="e42",
                    tool="tool",
                    regex=r"Invalid module item\.",
                    mode=MatchingMode.SPECIFIC,
                    title="",
                    profile="sv",
                ),
            ],
            extra_regexes=[r"syntax error$"],
        )
        result = analyze_command(
            "tool /tmp/x.sv",
            self._exec(output=output),
            cmd_cfg,
            ignored,
            "/tmp/x.sv",
            location_regex=r"^[A-z0-9_.\/,-]+:(\d+):",
        )
        self.assertEqual(result.outcome, "known_errors")
        ids = [m.error_id for m in result.matches]
        self.assertEqual(ids, ["e42"], f"Expected only KnownError id, got: {ids}")

    def test_grouping_extra_only_classified_as_known_errors_not_unknown(self):
        """Bare syntax error matched only by extra_ignored via grouping path.

        This mirrors the legacy ignored-only case: outcome must be known_errors,
        no unknown entry, and no .error_id access on the IgnoredError object.
        """
        output = "tmp/x.sv:5: syntax error\n"
        cmd_cfg = CommandConfig(
            run="tool {file}",
            error_regex=ToolErrorRegex(r"^[A-z0-9_.\/,-]+:\d+: .+$"),
        )
        ignored = IgnoredErrorsList.from_error_files(
            [],
            extra_regexes=[r"syntax error$"],
        )
        result = analyze_command(
            "tool /tmp/x.sv",
            self._exec(output=output),
            cmd_cfg,
            ignored,
            "/tmp/x.sv",
            location_regex=r"^[A-z0-9_.\/,-]+:(\d+):",
        )
        self.assertEqual(result.outcome, "known_errors")
        self.assertEqual(result.matches, [])


if __name__ == "__main__":
    unittest.main()
