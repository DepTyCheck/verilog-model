import json
import os
import tempfile
import unittest

from common.command_output import AnalyzisResult
from common.error_types import (
    ErrorMatchInTest,
    FoundMatch,
    KnownError,
    MatchingMode,
    UnexpectedError,
)
from tools_run.src.per_file_report import PerFileReport, _get_outcome


def _known_error() -> KnownError:
    return KnownError("err_undeclared", "pattern", MatchingMode.SPECIFIC)


class TestGetOutcome(unittest.TestCase):
    def test_clean(self):
        r = AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)
        self.assertEqual(_get_outcome(r), "clean")

    def test_timeout(self):
        r = AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True, timed_out=True)
        self.assertEqual(_get_outcome(r), "timeout")

    def test_unknown(self):
        r = AnalyzisResult(
            found_matches=[],
            unexpected_errors=[UnexpectedError("boom", "f.sv")],
            all_errors_are_known=False,
        )
        self.assertEqual(_get_outcome(r), "unknown")

    def test_known_errors(self):
        match = FoundMatch(error=_known_error(), matched_text="error: foo")
        r = AnalyzisResult(
            found_matches=[ErrorMatchInTest(match=match, test_path="f.sv")],
            unexpected_errors=[],
            all_errors_are_known=True,
        )
        self.assertEqual(_get_outcome(r), "known_errors")


class TestPerFileReport(unittest.TestCase):
    def _make_report(self) -> PerFileReport:
        return PerFileReport(
            tool_name="slang",
            tool_version="slang 7.0",
            tool_commit="abc123",
            model_commit="def456",
            run_date="2026-04-24",
        )

    def test_add_clean(self):
        report = self._make_report()
        r = AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)
        report.add_result("foo.sv", r)
        self.assertEqual(len(report._files), 1)
        self.assertEqual(report._files[0].outcome, "clean")
        self.assertEqual(report._files[0].matches, [])

    def test_add_timeout(self):
        report = self._make_report()
        r = AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True, timed_out=True)
        report.add_result("foo.sv", r)
        self.assertEqual(report._files[0].outcome, "timeout")

    def test_add_known_errors_captures_all_matches(self):
        report = self._make_report()
        err = _known_error()
        m1 = FoundMatch(error=err, matched_text="error: undeclared x")
        m2 = FoundMatch(error=err, matched_text="error: undeclared y")
        r = AnalyzisResult(
            found_matches=[
                ErrorMatchInTest(match=m1, test_path="foo.sv"),
                ErrorMatchInTest(match=m2, test_path="foo.sv"),
            ],
            unexpected_errors=[],
            all_errors_are_known=True,
        )
        report.add_result("foo.sv", r)
        rec = report._files[0]
        self.assertEqual(rec.outcome, "known_errors")
        self.assertEqual(len(rec.matches), 2)
        self.assertEqual(rec.matches[0].error_id, "err_undeclared")
        self.assertEqual(rec.matches[0].matched_text, "error: undeclared x")
        self.assertEqual(rec.matches[1].matched_text, "error: undeclared y")

    def test_ignored_error_match_not_recorded(self):
        """IgnoredError matches have no error_id; they must not appear in matches."""
        from common.error_types import IgnoredError

        report = self._make_report()
        ignored = IgnoredError(".*")
        match = FoundMatch(error=ignored, matched_text="anything")
        r = AnalyzisResult(
            found_matches=[ErrorMatchInTest(match=match, test_path="foo.sv")],
            unexpected_errors=[],
            all_errors_are_known=True,
        )
        report.add_result("foo.sv", r)
        self.assertEqual(report._files[0].matches, [])

    def test_save_produces_valid_json(self):
        report = self._make_report()
        r = AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)
        report.add_result("86-seed_111,222.sv", r)
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            path = f.name
        try:
            report.save(path)
            with open(path, encoding="utf-8") as f:
                data = json.load(f)
            self.assertEqual(data["tool_name"], "slang")
            self.assertEqual(data["tool_version"], "slang 7.0")
            self.assertEqual(data["tool_commit"], "abc123")
            self.assertEqual(data["model_commit"], "def456")
            self.assertEqual(data["run_date"], "2026-04-24")
            self.assertEqual(len(data["files"]), 1)
            self.assertEqual(data["files"][0]["filename"], "86-seed_111,222.sv")
            self.assertEqual(data["files"][0]["outcome"], "clean")
            self.assertEqual(data["files"][0]["matches"], [])
        finally:
            os.unlink(path)

    def test_save_includes_match_details(self):
        report = self._make_report()
        err = _known_error()
        match = FoundMatch(error=err, matched_text="error: foo; bar, baz")
        r = AnalyzisResult(
            found_matches=[ErrorMatchInTest(match=match, test_path="foo.sv")],
            unexpected_errors=[],
            all_errors_are_known=True,
        )
        report.add_result("foo.sv", r)
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            path = f.name
        try:
            report.save(path)
            with open(path, encoding="utf-8") as f:
                data = json.load(f)
            matches = data["files"][0]["matches"]
            self.assertEqual(len(matches), 1)
            self.assertEqual(matches[0]["error_id"], "err_undeclared")
            self.assertEqual(matches[0]["matched_text"], "error: foo; bar, baz")
        finally:
            os.unlink(path)


if __name__ == "__main__":
    unittest.main()
