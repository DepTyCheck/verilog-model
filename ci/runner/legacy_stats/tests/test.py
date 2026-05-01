from common.test_runner import run_test_suite

from .test_first_found_index import TestFirstFoundIndexErrors, TestFirstFoundIndexHappyPath
from .test_legacy_report import (
    TestLegacyReportBuild,
    TestLegacyReportCsv,
    TestLegacyReportErrors,
    TestLegacyReportSortTiebreak,
)
from .test_legacy_row import TestLegacyRow

if __name__ == "__main__":
    run_test_suite(
        [
            TestLegacyRow,
            TestFirstFoundIndexHappyPath,
            TestFirstFoundIndexErrors,
            TestLegacyReportBuild,
            TestLegacyReportSortTiebreak,
            TestLegacyReportErrors,
            TestLegacyReportCsv,
        ]
    )
