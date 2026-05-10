"""Unit tests for gen_matrix/gen_matrix.py."""

import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

from gen_matrix.gen_matrix import build_matrix, load_tools

# ci/runner/gen_matrix/tests/ -> parents[3] -> ci/
TOOLS_YAML = Path(__file__).parents[3] / "conf" / "tools.yaml"

_MINIMAL_YAML = """\
tools:
  - name: tool-a
    language: sv
    commands:
      - run: tool-a {file}
        error_regex: 'error: .*'
  - name: tool-b
    language: vhdl
    commands:
      - run: tool-b {file}
"""


class TestLoadTools(unittest.TestCase):

    def test_loads_real_tools_yaml(self):
        tools = load_tools(str(TOOLS_YAML))
        self.assertIsInstance(tools, list)
        self.assertGreater(len(tools), 0)

    def test_all_ten_tools_present(self):
        tools = load_tools(str(TOOLS_YAML))
        names = {t["name"] for t in tools}
        expected = {
            "iverilog",
            "slang",
            "verilator",
            "surelog",
            "zachjs-sv2v",
            "tree-sitter-systemverilog",
            "synlig",
            "sv_parser",
            "yosys-slang",
            "ghdl",
        }
        self.assertEqual(names, expected)

    def test_required_fields_present(self):
        tools = load_tools(str(TOOLS_YAML))
        for tool in tools:
            for field in ("name", "language", "commands"):
                self.assertIn(field, tool, f"Tool '{tool.get('name')}' missing field '{field}'")

    def test_commands_is_non_empty_list(self):
        tools = load_tools(str(TOOLS_YAML))
        for tool in tools:
            cmds = tool.get("commands")
            self.assertIsInstance(cmds, list, f"Tool '{tool['name']}' commands is not a list")
            self.assertGreater(len(cmds), 0, f"Tool '{tool['name']}' has empty commands list")
            for cmd in cmds:
                self.assertIn("run", cmd, f"Tool '{tool['name']}' command missing 'run' field")

    def test_empty_tools_list(self):
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("tools: []\n")
            tmp = f.name
        self.assertEqual(load_tools(tmp), [])

    def test_missing_tools_key(self):
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("{}\n")
            tmp = f.name
        self.assertEqual(load_tools(tmp), [])


class TestBuildMatrix(unittest.TestCase):

    def _parse_minimal(self) -> list[dict]:
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write(_MINIMAL_YAML)
            tmp = f.name
        return load_tools(tmp)

    def test_matrix_has_include_key(self):
        self.assertIn("include", build_matrix([]))

    def test_empty_tools_emits_empty_include(self):
        self.assertEqual(build_matrix([]), {"include": []})

    def test_tools_wrapped_under_tool_key(self):
        for item in build_matrix(self._parse_minimal())["include"]:
            self.assertIn("tool", item)
            self.assertIn("name", item["tool"])

    def test_all_tools_present_in_matrix(self):
        names = {item["tool"]["name"] for item in build_matrix(self._parse_minimal())["include"]}
        self.assertEqual(names, {"tool-a", "tool-b"})

    def test_matrix_is_json_serialisable(self):
        tools = load_tools(str(TOOLS_YAML))
        decoded = json.loads(json.dumps(build_matrix(tools)))
        self.assertEqual(len(decoded["include"]), len(tools))


class TestRoundTrip(unittest.TestCase):

    def test_error_regex_backslashes_preserved(self):
        tools = load_tools(str(TOOLS_YAML))
        decoded = json.loads(json.dumps(build_matrix(tools)))
        iverilog = next(i["tool"] for i in decoded["include"] if i["tool"]["name"] == "iverilog")
        first_cmd = iverilog["commands"][0]
        self.assertIn(r"\W", first_cmd["error_regex"])
        self.assertIn(r"\S", first_cmd["error_regex"])

    def test_multiline_build_commands_preserved(self):
        tools = load_tools(str(TOOLS_YAML))
        decoded = json.loads(json.dumps(build_matrix(tools)))
        verilator = next(i["tool"] for i in decoded["include"] if i["tool"]["name"] == "verilator")
        self.assertIn("\n", verilator["build"])

    def test_commands_array_survives_json_roundtrip(self):
        tools = load_tools(str(TOOLS_YAML))
        decoded = json.loads(json.dumps(build_matrix(tools)))
        for item in decoded["include"]:
            tool = item["tool"]
            self.assertIsInstance(tool["commands"], list)
            self.assertGreater(len(tool["commands"]), 0)
            for cmd in tool["commands"]:
                self.assertIn("run", cmd)


class TestMainOutput(unittest.TestCase):

    def test_main_writes_only_matrix_line(self):
        with tempfile.NamedTemporaryFile(mode="w", suffix=".out", delete=False) as f:
            output_path = f.name
        self.addCleanup(os.unlink, output_path)

        env = os.environ.copy()
        env["GITHUB_OUTPUT"] = output_path
        env["PYTHONPATH"] = str(Path(__file__).parents[2])

        subprocess.run(
            [sys.executable, "-m", "gen_matrix.main"],
            env=env,
            check=True,
            cwd=Path(__file__).parents[2],
        )

        with open(output_path, encoding="utf-8") as f:
            content = f.read()
        self.assertTrue(content.strip(), "gen_matrix.main wrote nothing to GITHUB_OUTPUT")
        lines = content.strip().split("\n")

        self.assertEqual(len(lines), 1, f"Expected 1 output line, got: {lines}")
        self.assertTrue(lines[0].startswith("matrix="))
        self.assertNotIn("check-matrix=", lines[0])
