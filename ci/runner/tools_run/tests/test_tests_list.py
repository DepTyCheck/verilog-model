"""
CommandConfig is defined in common/command_config.py.
These tests verify the dataclass contract hasn't changed.
"""

import unittest

from common.command_config import CommandConfig
from common.tool_error_regex import ToolErrorRegex


class TestCommandConfig(unittest.TestCase):

    def test_run_required(self):
        cc = CommandConfig(run="tool {file}")
        self.assertEqual(cc.run, "tool {file}")

    def test_error_regex_defaults_to_none(self):
        self.assertIsNone(CommandConfig(run="tool {file}").error_regex)

    def test_error_regex_can_be_set(self):
        regex = ToolErrorRegex("error: .*")
        cc = CommandConfig(run="tool {file}", error_regex=regex)
        self.assertIs(cc.error_regex, regex)


if __name__ == "__main__":
    unittest.main()
