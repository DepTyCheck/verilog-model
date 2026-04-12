import logging
import unittest
from pathlib import Path

from common.logger import configure_logger
from tools_run.src.assets import Assets
from tools_run.src.tests_list import CommandConfig, TestsList
from tools_run.tests.test_parsing_ignored_errors_list import create_ignored_errors_list


class TestAssets(unittest.TestCase):
    def test_assets_copying(self):
        asset_path = "tools_run/tests/data/ignored_errors/ignored_error_2.yaml"
        test_file_path = "tools_run/tests/data/generated-modules.sv"

        configure_logger(level=logging.ERROR)

        ignored = create_ignored_errors_list(tool="someTool")

        runner = TestsList(
            files=[Path(test_file_path)],
            ignored_errors_list=ignored,
            commands=[CommandConfig(run=f"cat {asset_path}")],
            assets=Assets([asset_path]),
        )

        res = runner.run_all()

        self.assertFalse(res.has_unexpected_errors())
