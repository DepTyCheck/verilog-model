from common.test_runner import run_test_suite

from .test_combined_report import (
    TestCombinedReportBuild,
    TestCombinedReportCsv,
    TestCombinedReportErrors,
    TestCombinedReportLastOccurrenceTiebreak,
    TestCombinedReportZeroRuns,
)
from .test_combined_row import TestCombinedRow
from .test_files_index import TestFilesIndex, TestFilesIndexBadName
from .test_first_found_index import (
    TestFirstFoundIndexErrors,
    TestFirstFoundIndexHappyPath,
)
from .test_issues_index import TestIssuesIndex
from .test_legacy_index import TestLegacyIndex

if __name__ == "__main__":
    run_test_suite(
        [
            TestCombinedRow,
            TestFirstFoundIndexHappyPath,
            TestFirstFoundIndexErrors,
            TestIssuesIndex,
            TestFilesIndex,
            TestFilesIndexBadName,
            TestLegacyIndex,
            TestCombinedReportBuild,
            TestCombinedReportErrors,
            TestCombinedReportLastOccurrenceTiebreak,
            TestCombinedReportZeroRuns,
            TestCombinedReportCsv,
        ]
    )
