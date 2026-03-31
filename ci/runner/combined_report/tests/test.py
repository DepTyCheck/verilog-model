import sys
import unittest

from .test_combined_report import TestCombinedReport
from .test_percentages import TestOccurrencePct, TestTotalTestCount
from .test_previous import TestPreviousReport
from .test_tools_report import TestToolsReport
from .test_tools_report_list import TestToolsReportList

if __name__ == "__main__":
    suite = unittest.TestSuite()

    loader = unittest.TestLoader()
    test_cases = [
        TestOccurrencePct,
        TestTotalTestCount,
        TestPreviousReport,
        TestToolsReport,
        TestToolsReportList,
        TestCombinedReport,
    ]
    for test_case in test_cases:
        suite.addTests(loader.loadTestsFromTestCase(test_case))

    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    sys.exit(1 if result.errors or result.failures else 0)
