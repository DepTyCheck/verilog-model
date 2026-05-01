import json
import os
import tempfile
import unittest

from compare_errors.current_index import CurrentIndex


def _write_per_file(dir_path: str, tool_name: str, files: list) -> None:
    payload = {
        "tool_name": tool_name,
        "tool_version": "v",
        "tool_commit": "c",
        "model_commit": "m",
        "run_date": "2026-04-30",
        "files": files,
    }
    with open(os.path.join(dir_path, f"{tool_name}-per-file.json"), "w", encoding="utf-8") as fh:
        json.dump(payload, fh)


def _file(filename: str, *match_pairs: tuple[str, str]) -> dict:
    return {
        "filename": filename,
        "commands": [
            {
                "command": "tool args",
                "outcome": "known_errors" if match_pairs else "clean",
                "matches": [{"error_id": eid, "matched_text": text} for eid, text in match_pairs],
            }
        ],
    }


class TestCurrentIndex(unittest.TestCase):
    def test_error_ids_collected_from_all_files(self):
        with tempfile.TemporaryDirectory() as tmp:
            _write_per_file(
                tmp,
                "tool_a",
                [
                    _file("f1.sv", ("err_a", "x")),
                    _file("f2.sv"),
                ],
            )
            _write_per_file(
                tmp,
                "tool_b",
                [
                    _file("f1.vhdl", ("err_b", "y")),
                ],
            )
            idx = CurrentIndex(tmp)
        self.assertEqual(idx.error_ids(), {"err_a", "err_b"})

    def test_pct_counts_every_match_record(self):
        # tool_a runs over 2 files; err_a appears in 3 match records total (one
        # file has 2 hits in one command, the other file has 1 hit).
        with tempfile.TemporaryDirectory() as tmp:
            _write_per_file(
                tmp,
                "tool_a",
                [
                    _file("f1.sv", ("err_a", "x"), ("err_a", "x")),
                    _file("f2.sv", ("err_a", "x")),
                ],
            )
            idx = CurrentIndex(tmp)
        self.assertAlmostEqual(idx.current_pct("err_a"), 3.0 / 2.0 * 100.0)

    def test_pct_uses_owning_tool_denominator(self):
        # err_a belongs to tool_a (3 files); err_b belongs to tool_b (10 files).
        with tempfile.TemporaryDirectory() as tmp:
            _write_per_file(
                tmp,
                "tool_a",
                [
                    _file("a1.sv", ("err_a", "x")),
                    _file("a2.sv"),
                    _file("a3.sv"),
                ],
            )
            _write_per_file(tmp, "tool_b", [_file(f"b{i}.vhdl") for i in range(9)] + [_file("b9.vhdl", ("err_b", "y"))])
            idx = CurrentIndex(tmp)
        self.assertAlmostEqual(idx.current_pct("err_a"), 1.0 / 3.0 * 100.0)
        self.assertAlmostEqual(idx.current_pct("err_b"), 1.0 / 10.0 * 100.0)

    def test_unknown_sentinel_is_skipped(self):
        with tempfile.TemporaryDirectory() as tmp:
            _write_per_file(
                tmp,
                "tool_a",
                [
                    _file("f1.sv", ("unknown", "boom")),
                    _file("f2.sv", ("err_a", "x")),
                ],
            )
            idx = CurrentIndex(tmp)
        self.assertEqual(idx.error_ids(), {"err_a"})
        with self.assertRaises(KeyError):
            idx.current_pct("unknown")

    def test_unknown_error_id_raises(self):
        with tempfile.TemporaryDirectory() as tmp:
            _write_per_file(tmp, "tool_a", [_file("f1.sv")])
            idx = CurrentIndex(tmp)
        with self.assertRaises(KeyError):
            idx.current_pct("does_not_exist")

    def test_empty_dir_yields_no_ids(self):
        with tempfile.TemporaryDirectory() as tmp:
            idx = CurrentIndex(tmp)
        self.assertEqual(idx.error_ids(), set())

    def test_zero_files_in_report_yields_no_ids(self):
        with tempfile.TemporaryDirectory() as tmp:
            # A tool that registers an empty files: [] payload produces no
            # error_ids in the index. The ValueError guard inside current_pct
            # (zero-file denominator) is then unreachable through the public
            # API — it's defence in depth.
            _write_per_file(tmp, "tool_a", [])
            idx = CurrentIndex(tmp)
        # No error_id reaches the index, so error_ids() is empty.
        self.assertEqual(idx.error_ids(), set())

    def test_duplicate_tool_name_across_json_raises(self):
        with tempfile.TemporaryDirectory() as tmp:
            _write_per_file(tmp, "tool_a", [_file("f1.sv")])
            # Second JSON for the same tool — the writer must invent a different
            # filename, since _write_per_file derives it from tool_name. Write it
            # manually.
            payload = {
                "tool_name": "tool_a",
                "tool_version": "v",
                "tool_commit": "c",
                "model_commit": "m",
                "run_date": "2026-04-30",
                "files": [_file("f2.sv")],
            }
            with open(os.path.join(tmp, "tool_a-extra-per-file.json"), "w", encoding="utf-8") as fh:
                json.dump(payload, fh)
            with self.assertRaises(ValueError):
                CurrentIndex(tmp)
