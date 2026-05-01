import unittest

from dataset_builder.src.filter import passing_filenames
from dataset_builder.src.per_file_report import FileRecord, ToolReport


def _report(tool: str, files: list[tuple[str, str]]) -> ToolReport:
    """Build a ToolReport from (filename, outcome) pairs."""
    return ToolReport(
        tool_name=tool,
        tool_version="v1",
        tool_commit="abc",
        model_commit="def",
        run_date="2026-04-24",
        files=[FileRecord(filename=fn, outcome=oc, matches=[]) for fn, oc in files],
    )


class TestPassingFilenames(unittest.TestCase):
    def test_all_clean_pass(self):
        r = _report("slang", [("a.sv", "clean"), ("b.sv", "clean")])
        self.assertEqual(passing_filenames([r]), {"a.sv", "b.sv"})

    def test_known_errors_passes(self):
        r = _report("slang", [("a.sv", "known_errors")])
        self.assertEqual(passing_filenames([r]), {"a.sv"})

    def test_timeout_excluded(self):
        r = _report("slang", [("a.sv", "timeout"), ("b.sv", "clean")])
        result = passing_filenames([r])
        self.assertNotIn("a.sv", result)
        self.assertIn("b.sv", result)

    def test_unknown_excluded(self):
        r = _report("slang", [("a.sv", "unknown"), ("b.sv", "clean")])
        result = passing_filenames([r])
        self.assertNotIn("a.sv", result)

    def test_file_fails_on_one_tool_excluded(self):
        r1 = _report("slang", [("a.sv", "clean"), ("b.sv", "clean")])
        r2 = _report("iverilog", [("a.sv", "unknown"), ("b.sv", "clean")])
        result = passing_filenames([r1, r2])
        self.assertNotIn("a.sv", result)
        self.assertIn("b.sv", result)

    def test_file_only_in_one_tool_still_considered(self):
        """sv files don't appear in vhdl tool reports — should still pass."""
        r_sv = _report("slang", [("a.sv", "clean")])
        r_vhdl = _report("ghdl", [("b.vhdl", "clean")])
        result = passing_filenames([r_sv, r_vhdl])
        self.assertIn("a.sv", result)
        self.assertIn("b.vhdl", result)

    def test_empty_reports(self):
        self.assertEqual(passing_filenames([]), set())

    def test_mixed_outcomes_same_file_multiple_tools(self):
        r1 = _report("slang", [("a.sv", "known_errors")])
        r2 = _report("iverilog", [("a.sv", "known_errors")])
        r3 = _report("verilator", [("a.sv", "timeout")])
        result = passing_filenames([r1, r2, r3])
        self.assertNotIn("a.sv", result)


if __name__ == "__main__":
    unittest.main()
