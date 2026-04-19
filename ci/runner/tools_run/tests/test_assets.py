import logging
import unittest
from pathlib import Path

from common.command_config import CommandConfig
from common.logger import configure_logger
from common.tool_matrix_runner import ResultCollector, run_all
from tools_run.src.assets import Assets
from tools_run.src.input_iter import iter_test_files
from tools_run.tests.test_parsing_ignored_errors_list import create_ignored_errors_list


class TestAssets(unittest.TestCase):
    def test_assets_copying(self):
        asset_path = "tools_run/tests/data/ignored_errors/ignored_error_2.yaml"
        test_file_path = "tools_run/tests/data/generated-modules.sv"

        configure_logger(level=logging.ERROR)

        ignored = create_ignored_errors_list(tool="someTool")
        assets = Assets([asset_path])
        commands = [CommandConfig(run=f"cat {asset_path}")]

        files = [Path(test_file_path)]
        inputs = list(iter_test_files(files, ".sv", assets))

        collector = ResultCollector()
        run_all(inputs, commands, ignored, collector)

        results = collector.results()
        self.assertTrue(len(results) > 0)
        for _, result in results:
            self.assertEqual(result.unexpected_errors, [])
