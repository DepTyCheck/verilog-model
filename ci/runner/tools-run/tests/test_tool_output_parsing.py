import unittest

from src.command_output import CommandOutput
from src.tool_error_regex import ToolErrorRegex

from .test_parsing_ignored_errors_list import create_ignored_errors_list


class TestToolOutputParsing(unittest.TestCase):

    def test_tool_output_parsing(self):
        ignored_errors_list = create_ignored_errors_list(tool="verilator")

        with open("tests/data/verilator_sim_output.txt", encoding="utf-8") as f:
            res = CommandOutput(f.read()).analyze(
                ignored_errors_list,
                tool_error_regex=ToolErrorRegex(r"%Error[-A-z]*:[\S ]+"),
                file_path="does_not_matter",
            )
            print(res.found_matches)
            print(res.unexpected_errors)
            print(res.all_errors_are_known)
