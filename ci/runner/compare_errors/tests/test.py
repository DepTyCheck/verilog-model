import sys
import unittest

from .test_compare_errors import TestErrorPercentageDelta, TestErrorsComparison
from .test_table_formatter import TestTableFormatter

if __name__ == "__main__":
    suite = unittest.TestSuite()

    loader = unittest.TestLoader()
    test_cases = [
        TestErrorPercentageDelta,
        TestErrorsComparison,
        TestTableFormatter,
    ]
    for test_case in test_cases:
        suite.addTests(loader.loadTestsFromTestCase(test_case))

    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    sys.exit(1 if result.errors or result.failures else 0)
