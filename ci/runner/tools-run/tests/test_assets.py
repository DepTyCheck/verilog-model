import logging
import unittest
from pathlib import Path

from src.assets import Assets
from src.logger import configure_logger
from src.tests_list import TestsList
from src.tool_error_regex import ToolErrorRegex

from .test_parsing_ignored_errors_list import create_ignored_errors_list


class TestAssets(unittest.TestCase):
    def test_assets_copying(self):
        asset_path = "tests/data/ignored_errors/ignored_error_2.yaml"
        test_file_path = "tests/data/generated-modules.sv"

        configure_logger(level=logging.ERROR)

        ignored = create_ignored_errors_list(tool="someTool")

        runner = TestsList(
            files=[Path(test_file_path)],
            ignored_errors_list=ignored,
            main_error_regex=ToolErrorRegex(""),
            sim_error_regex=None,
            raw_synth_cmd=f"cat {asset_path}",
            raw_sim_cmd=None,
            assets=Assets([asset_path]),
        )

        res = runner.run_all()

        self.assertFalse(res.has_unexpected_errors())
