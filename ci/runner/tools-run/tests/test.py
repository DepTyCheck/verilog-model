import unittest

from .test_assets import TestAssets
from .test_cwd import TestRunCommandCwd
from .test_parsing_ignored_errors_list import TestIgnoredErrorsList
from .test_root_entity_regex import TestRootEntityRegex
from .test_tool_output_parsing import TestToolOutputParsing

if __name__ == "__main__":
    suite = unittest.TestSuite()

    loader = unittest.TestLoader()
    test_cases = [
        TestIgnoredErrorsList,
        TestToolOutputParsing,
        TestRunCommandCwd,
        TestRootEntityRegex,
        TestAssets,
    ]
    for test_case in test_cases:
        suite.addTests(loader.loadTestsFromTestCase(test_case))

    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite)
