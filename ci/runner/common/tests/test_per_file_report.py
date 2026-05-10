# ci/runner/common/tests/test_per_file_report.py
import json
import os
import tempfile
import unittest

from common.per_file_report import (
    CommandRecord,
    FileRecord,
    MatchRecord,
    PerFileReport,
    file_outcome,
    load_report,
)


def _cmd(outcome: str, *matches: tuple[str, str]) -> CommandRecord:
    return CommandRecord(
        command=f"tool --outcome={outcome}",
        outcome=outcome,
        matches=[MatchRecord(error_id=eid, matched_text=text) for eid, text in matches],
    )


class TestFileOutcome(unittest.TestCase):
    def test_all_clean(self):
        rec = FileRecord(filename="a.sv", commands=[_cmd("clean"), _cmd("clean")])
        self.assertEqual(file_outcome(rec), "clean")

    def test_known_errors(self):
        rec = FileRecord(filename="a.sv", commands=[_cmd("clean"), _cmd("known_errors", ("e1", "boom"))])
        self.assertEqual(file_outcome(rec), "known_errors")

    def test_unknown_dominates_known(self):
        rec = FileRecord(
            filename="a.sv",
            commands=[_cmd("known_errors", ("e1", "boom")), _cmd("unknown", ("unknown", "whoops"))],
        )
        self.assertEqual(file_outcome(rec), "unknown")

    def test_timeout_dominates_unknown(self):
        rec = FileRecord(
            filename="a.sv",
            commands=[_cmd("unknown", ("unknown", "whoops")), _cmd("timeout")],
        )
        self.assertEqual(file_outcome(rec), "timeout")


class TestPerFileReportRoundtrip(unittest.TestCase):
    def test_save_load_roundtrip(self):
        report = PerFileReport(
            tool_name="iverilog",
            tool_version="12.0",
            tool_commit="abc",
            model_commit="def",
            run_date="2026-04-26",
        )
        report.add_file(
            FileRecord(
                filename="t1.sv",
                commands=[
                    _cmd("known_errors", ("err_x", "error: x")),
                ],
            )
        )
        report.add_file(FileRecord(filename="t2.sv", commands=[_cmd("clean")]))

        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as fh:
            path = fh.name
        try:
            report.save(path)

            with open(path, encoding="utf-8") as fh:
                raw = json.load(fh)
            self.assertEqual(raw["tool_name"], "iverilog")
            self.assertEqual(len(raw["files"]), 2)
            self.assertNotIn("outcome", raw["files"][0])  # file-level outcome is not stored
            self.assertEqual(raw["files"][0]["commands"][0]["outcome"], "known_errors")
            self.assertEqual(raw["files"][0]["commands"][0]["matches"][0]["error_id"], "err_x")

            loaded = load_report(path)
            self.assertEqual(loaded.tool_name, "iverilog")
            self.assertEqual(len(loaded.files), 2)
            self.assertEqual(file_outcome(loaded.files[0]), "known_errors")
            self.assertEqual(file_outcome(loaded.files[1]), "clean")
        finally:
            os.unlink(path)


if __name__ == "__main__":
    unittest.main()
