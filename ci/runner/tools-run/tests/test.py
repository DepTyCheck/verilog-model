import unittest

from .test_parsing_ignored_errors_list import TestIgnoredErrorsList

if __name__ == "__main__":
    suite = unittest.TestSuite()

    loader = unittest.TestLoader()
    test_cases = [
        TestIgnoredErrorsList,
    ]
    for test_case in test_cases:
        suite.addTests(loader.loadTestsFromTestCase(test_case))

    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite)
