from common.test_runner import run_test_suite

from .test_combined_report import TestCombinedReport
from .test_percentages import TestOccurrencePct, TestTotalTestCount
from .test_previous import TestPreviousReport
from .test_tools_report import TestToolsReport
from .test_tools_report_list import TestToolsReportList

if __name__ == "__main__":
    run_test_suite(
        [
            TestOccurrencePct,
            TestTotalTestCount,
            TestPreviousReport,
            TestToolsReport,
            TestToolsReportList,
            TestCombinedReport,
        ]
    )
