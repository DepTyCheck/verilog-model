"""
Unit tests for regression_test/src/error_checker.py.

All tests are pure-Python: no real tool execution happens.
"""

import os
import tempfile
import textwrap
import unittest
from unittest.mock import MagicMock

from common.error_file_parser import ErrorFile, Example
from common.error_types import MatchingMode
from regression_test.src.error_checker import iter_regression_inputs, load_all_error_files

# ---------------------------------------------------------------------------
# Helpers for load_all_error_files tests
# ---------------------------------------------------------------------------

_TOOL_A_YAML = textwrap.dedent("""\
    id: error_a
    tool: tool-a
    regex: 'known error from a'
    examples:
      - ex1:
          minified_example: "module bad(); endmodule"
""")

_TOOL_B_YAML = textwrap.dedent("""\
    id: error_b
    tool: tool-b
    regex: 'known error from b'
    examples:
      - ex1:
          minified_example: "entity bad is end bad;"
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


def _write_known_errors_dir(tmp: str, files: dict[str, str]) -> str:
    for rel_path, content in files.items():
        full_path = os.path.join(tmp, rel_path)
        os.makedirs(os.path.dirname(full_path), exist_ok=True)
        with open(full_path, "w", encoding="utf-8") as f:
            f.write(content)
    return tmp


def _make_ef(language: str = "sv") -> ErrorFile:
    return ErrorFile(
        error_id="e1",
        tool="t",
        regex="p",
        mode=MatchingMode.SPECIFIC,
        title="",
        language=language,
        examples=[Example(name="ex1", type="minified", content="c")],
    )


class TestLoadAllErrorFiles(unittest.TestCase):

    def test_loads_from_all_subdirs(self):
        with tempfile.TemporaryDirectory() as tmp:
            _write_known_errors_dir(tmp, {"tool-a/error_a.yaml": _TOOL_A_YAML, "tool-b/error_b.yaml": _TOOL_B_YAML})
            files = load_all_error_files(tmp)
        self.assertEqual({f.error_id for f in files}, {"error_a", "error_b"})

    def test_returns_empty_for_missing_dir(self):
        self.assertEqual(load_all_error_files("/nonexistent/path"), [])

    def test_originating_tool_set_from_yaml(self):
        with tempfile.TemporaryDirectory() as tmp:
            _write_known_errors_dir(tmp, {"tool-a/error_a.yaml": _TOOL_A_YAML})
            files = load_all_error_files(tmp)
        self.assertEqual(files[0].tool, "tool-a")


# ---------------------------------------------------------------------------
# iter_regression_inputs
# ---------------------------------------------------------------------------


class TestIterRegressionInputs(unittest.TestCase):

    def test_yields_one_input_per_example(self):
        ef = ErrorFile(
            error_id="e1",
            tool="t",
            regex="pat",
            mode=MatchingMode.SPECIFIC,
            title="",
            language="sv",
            examples=[
                Example(name="ex1", type="minified", content="module m; endmodule"),
                Example(name="ex2", type="full", content="module n; endmodule"),
            ],
        )
        inputs = list(iter_regression_inputs([ef], ".sv"))
        self.assertEqual(len(inputs), 2)

    def test_context_is_error_file_and_example_tuple(self):
        example = Example(name="ex1", type="minified", content="abc")
        ef = ErrorFile(
            error_id="e1",
            tool="t",
            regex="p",
            mode=MatchingMode.SPECIFIC,
            title="",
            language="sv",
            examples=[example],
        )
        inputs = list(iter_regression_inputs([ef], ".sv"))
        ctx_ef, ctx_ex = inputs[0].context
        self.assertIs(ctx_ef, ef)
        self.assertIs(ctx_ex, example)

    def test_content_and_suffix_set_correctly(self):
        ef = ErrorFile(
            error_id="e1",
            tool="t",
            regex="p",
            mode=MatchingMode.SPECIFIC,
            title="",
            language="vhdl",
            examples=[Example(name="ex1", type="minified", content="my content")],
        )
        inputs = list(iter_regression_inputs([ef], ".vhdl"))
        self.assertEqual(inputs[0].content, "my content")
        self.assertEqual(inputs[0].file_suffix, ".vhdl")

    def test_logical_name_is_error_id_slash_example_name(self):
        ef = ErrorFile(
            error_id="e1",
            tool="t",
            regex="p",
            mode=MatchingMode.SPECIFIC,
            title="",
            language="sv",
            examples=[Example(name="ex1", type="minified", content="c")],
        )
        inputs = list(iter_regression_inputs([ef], ".sv"))
        self.assertEqual(inputs[0].logical_name, "e1/ex1")

    def test_assets_propagated_to_each_input(self):
        ef = ErrorFile(
            error_id="e1",
            tool="t",
            regex="p",
            mode=MatchingMode.SPECIFIC,
            title="",
            language="sv",
            examples=[
                Example(name="ex1", type="minified", content="a"),
                Example(name="ex2", type="full", content="b"),
            ],
        )
        assets = MagicMock()
        inputs = list(iter_regression_inputs([ef], ".sv", assets=assets))
        for inp in inputs:
            self.assertIs(inp.assets, assets)

    def test_language_filter_matches(self):
        inputs = list(iter_regression_inputs([_make_ef("vhdl")], ".vhdl", language="vhdl"))
        self.assertEqual(len(inputs), 1)

    def test_language_filter_excludes_wrong_language(self):
        inputs = list(iter_regression_inputs([_make_ef("sv")], ".vhdl", language="vhdl"))
        self.assertEqual(len(inputs), 0)

    def test_no_language_filter_yields_all(self):
        inputs = list(iter_regression_inputs([_make_ef("sv"), _make_ef("vhdl")], ".sv"))
        self.assertEqual(len(inputs), 2)
