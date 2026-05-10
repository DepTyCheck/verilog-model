# ci/runner/regression_analyze/tests/test_reproducibility.py
import unittest

from common.per_file_report import CommandRecord, FileRecord, MatchRecord, PerFileReport
from regression_analyze.reproducibility import (
    build_reproducibility_table,
    parse_example_filename,
)


def _file(filename: str, *match_pairs: tuple[str, str]) -> FileRecord:
    matches = [MatchRecord(error_id=eid, matched_text=text) for eid, text in match_pairs]
    return FileRecord(
        filename=filename,
        commands=[CommandRecord(command="tool", outcome="known_errors" if matches else "clean", matches=matches)],
    )


def _report(files: list[FileRecord]) -> PerFileReport:
    return PerFileReport(
        tool_name="iverilog",
        tool_version="v",
        tool_commit="c",
        model_commit="m",
        run_date="d",
        files=files,
    )


class TestParseExampleFilename(unittest.TestCase):
    def test_minified(self):
        self.assertEqual(parse_example_filename("foo-minified.sv"), ("foo", "minified"))

    def test_full(self):
        self.assertEqual(parse_example_filename("nested-name-full.vhdl"), ("nested-name", "full"))

    def test_unrecognised_returns_none(self):
        self.assertIsNone(parse_example_filename("garbage.sv"))


class TestBuildReproducibilityTable(unittest.TestCase):
    def test_reproduced_when_match_id_equals_expected(self):
        expected = {("foo", "minified"): "err_a"}
        report = _report([_file("foo-minified.sv", ("err_a", "boom"))])
        table = build_reproducibility_table(report, expected)
        self.assertEqual(table, {"err_a": {"foo-minified": True}})

    def test_not_reproduced_when_match_missing(self):
        expected = {("foo", "minified"): "err_a"}
        report = _report([_file("foo-minified.sv")])
        table = build_reproducibility_table(report, expected)
        self.assertEqual(table, {"err_a": {"foo-minified": False}})

    def test_not_reproduced_when_match_id_mismatches(self):
        expected = {("foo", "minified"): "err_a"}
        report = _report([_file("foo-minified.sv", ("err_b", "boom"))])
        table = build_reproducibility_table(report, expected)
        self.assertEqual(table, {"err_a": {"foo-minified": False}})

    def test_unmapped_file_skipped(self):
        expected = {("foo", "minified"): "err_a"}
        report = _report([_file("ghost-minified.sv", ("err_b", "boom"))])
        table = build_reproducibility_table(report, expected)
        self.assertEqual(table, {})


if __name__ == "__main__":
    unittest.main()
