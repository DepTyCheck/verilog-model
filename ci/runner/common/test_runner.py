import sys
import unittest


def run_test_suite(test_cases: list) -> None:
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    for test_case in test_cases:
        suite.addTests(loader.loadTestsFromTestCase(test_case))
    result = unittest.TextTestRunner(verbosity=2).run(suite)
    sys.exit(1 if result.errors or result.failures else 0)
