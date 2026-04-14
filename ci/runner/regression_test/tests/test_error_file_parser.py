"""Unit tests for common/error_file_parser.py — exercises the examples field parsing."""

import os
import tempfile
import textwrap
import unittest
from pathlib import Path

from common.error_file_parser import parse_error_files
from common.error_types import MatchingMode

DATA_DIR = str(Path(__file__).parent / "data")


class TestErrorFileParser(unittest.TestCase):

    def test_parses_yaml_with_examples(self):
        files = parse_error_files(DATA_DIR, tool="fake_tool")
        # Should find still_reproducing and fixed_bug (tool=fake_tool) plus new_error_trigger
        error_ids = {f.error_id for f in files}
        self.assertIn("still_reproducing_error", error_ids)
        self.assertIn("fixed_bug_error", error_ids)

    def test_examples_loaded_for_still_reproducing(self):
        files = parse_error_files(DATA_DIR, tool="fake_tool")
        target = next(f for f in files if f.error_id == "still_reproducing_error")
        # Should have both minified and full examples from the one example block
        types = {e.type for e in target.examples}
        self.assertIn("minified", types)
        self.assertIn("full", types)
        self.assertEqual(len(target.examples), 2)

    def test_minified_example_content_is_correct(self):
        files = parse_error_files(DATA_DIR, tool="fake_tool")
        target = next(f for f in files if f.error_id == "still_reproducing_error")
        minified = next(e for e in target.examples if e.type == "minified")
        self.assertIn("module broken", minified.content)

    def test_fixed_bug_has_only_minified(self):
        files = parse_error_files(DATA_DIR, tool="fake_tool")
        target = next(f for f in files if f.error_id == "fixed_bug_error")
        self.assertEqual(len(target.examples), 1)
        self.assertEqual(target.examples[0].type, "minified")

    def test_tool_filter_excludes_other_tools(self):
        files = parse_error_files(DATA_DIR, tool="other_tool")
        self.assertEqual(files, [])

    def test_no_filter_returns_all(self):
        files = parse_error_files(DATA_DIR)
        self.assertGreaterEqual(len(files), 3)

    def test_matching_mode_defaults_to_specific(self):
        files = parse_error_files(DATA_DIR, tool="fake_tool")
        target = next(f for f in files if f.error_id == "still_reproducing_error")
        self.assertEqual(target.mode, MatchingMode.SPECIFIC)

    def test_nonexistent_directory_returns_empty(self):
        files = parse_error_files("/nonexistent/path/for/test")
        self.assertEqual(files, [])

    def test_two_examples_with_full_loads_four_example_objects(self):
        files = parse_error_files(DATA_DIR, tool="fake_tool")
        target = next(f for f in files if f.error_id == "two_examples_with_full")
        # 2 example blocks × 2 types (minified + full) = 4 Example objects
        self.assertEqual(len(target.examples), 4)

    def test_two_examples_with_full_has_both_types_for_each_name(self):
        files = parse_error_files(DATA_DIR, tool="fake_tool")
        target = next(f for f in files if f.error_id == "two_examples_with_full")
        by_name: dict[str, list[str]] = {}
        for ex in target.examples:
            by_name.setdefault(ex.name, []).append(ex.type)
        self.assertEqual(set(by_name.keys()), {"example_1", "example_2"})
        self.assertEqual(sorted(by_name["example_1"]), ["full", "minified"])
        self.assertEqual(sorted(by_name["example_2"]), ["full", "minified"])

    def test_two_examples_with_full_content_is_distinct(self):
        files = parse_error_files(DATA_DIR, tool="fake_tool")
        target = next(f for f in files if f.error_id == "two_examples_with_full")
        contents = [ex.content for ex in target.examples]
        # All four example contents are distinct
        self.assertEqual(len(set(contents)), 4)

    def test_lang_field_read_from_yaml(self):
        """Files with an explicit 'lang' field are stored in ErrorFile.language."""
        yaml_content = textwrap.dedent("""\
            id: vhdl_error
            tool: ghdl
            lang: vhdl
            regex: 'error: .*'
            examples: []
        """)
        with tempfile.TemporaryDirectory() as tmp:
            with open(os.path.join(tmp, "err.yaml"), "w") as f:
                f.write(yaml_content)
            files = parse_error_files(tmp)
        self.assertEqual(files[0].language, "vhdl")

    def test_lang_defaults_to_sv_when_absent(self):
        """Files without 'lang' default to 'sv' (all existing SV tool error files)."""
        files = parse_error_files(DATA_DIR, tool="fake_tool")
        for f in files:
            self.assertEqual(f.language, "sv")
