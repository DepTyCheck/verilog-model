import json
import os
import tempfile
import unittest

from dataset_builder.src.per_file_report import MatchRecord, ToolReport, load_report


class TestLoadReport(unittest.TestCase):
    def _write_json(self, data: dict) -> str:
        f = tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False)
        json.dump(data, f)
        f.close()
        return f.name

    def _sample_data(self) -> dict:
        return {
            "tool_name": "slang",
            "tool_version": "slang 7.0",
            "tool_commit": "abc123",
            "model_commit": "def456",
            "run_date": "2026-04-24",
            "files": [
                {"filename": "86-seed_111,222.sv", "outcome": "clean", "matches": []},
                {
                    "filename": "191-seed_333,444.sv",
                    "outcome": "known_errors",
                    "matches": [
                        {"error_id": "err_foo", "matched_text": "error: foo"},
                        {"error_id": "err_foo", "matched_text": "error: bar"},
                    ],
                },
                {"filename": "77-seed_555,666.sv", "outcome": "timeout", "matches": []},
                {"filename": "10-seed_777,888.sv", "outcome": "unknown", "matches": []},
            ],
        }

    def setUp(self):
        from pathlib import Path

        data = self._sample_data()
        self.path = self._write_json(data)
        self.report = load_report(Path(self.path))

    def tearDown(self):
        os.unlink(self.path)

    def test_tool_metadata(self):
        self.assertEqual(self.report.tool_name, "slang")
        self.assertEqual(self.report.tool_version, "slang 7.0")
        self.assertEqual(self.report.tool_commit, "abc123")
        self.assertEqual(self.report.model_commit, "def456")
        self.assertEqual(self.report.run_date, "2026-04-24")

    def test_file_count(self):
        self.assertEqual(len(self.report.files), 4)

    def test_clean_file(self):
        clean = self.report.files[0]
        self.assertEqual(clean.filename, "86-seed_111,222.sv")
        self.assertEqual(clean.outcome, "clean")
        self.assertEqual(clean.matches, [])

    def test_known_errors_file(self):
        rec = self.report.files[1]
        self.assertEqual(rec.outcome, "known_errors")
        self.assertEqual(len(rec.matches), 2)
        self.assertEqual(rec.matches[0], MatchRecord(error_id="err_foo", matched_text="error: foo"))
        self.assertEqual(rec.matches[1], MatchRecord(error_id="err_foo", matched_text="error: bar"))

    def test_timeout_file(self):
        self.assertEqual(self.report.files[2].outcome, "timeout")

    def test_unknown_file(self):
        self.assertEqual(self.report.files[3].outcome, "unknown")


if __name__ == "__main__":
    unittest.main()
