import sys
import unittest

from .test_count_lines import TestCountFile, TestCountLines, TestFormatReport, TestIsPrinterFile, TestIsPureCodeLine

if __name__ == "__main__":
    suite = unittest.TestSuite()
    loader = unittest.TestLoader()
    test_cases = [
        TestIsPrinterFile,
        TestIsPureCodeLine,
        TestCountFile,
        TestCountLines,
        TestFormatReport,
    ]
    for test_case in test_cases:
        suite.addTests(loader.loadTestsFromTestCase(test_case))

    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    sys.exit(1 if result.errors or result.failures else 0)
