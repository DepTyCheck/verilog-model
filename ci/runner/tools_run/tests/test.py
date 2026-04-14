from common.test_runner import run_test_suite
from tools_run.tests.test_assets import TestAssets
from tools_run.tests.test_cwd import TestRunCommandCwd
from tools_run.tests.test_parsing_ignored_errors_list import TestIgnoredErrorsList
from tools_run.tests.test_print_stats import TestCountRunStats
from tools_run.tests.test_root_entity_regex import TestRootEntityRegex
from tools_run.tests.test_tests_list import TestCommandConfig
from tools_run.tests.test_tool_output_parsing import TestToolOutputParsing

if __name__ == "__main__":
    run_test_suite(
        [
            TestIgnoredErrorsList,
            TestToolOutputParsing,
            TestRunCommandCwd,
            TestRootEntityRegex,
            TestAssets,
            TestCountRunStats,
            TestCommandConfig,
        ]
    )
