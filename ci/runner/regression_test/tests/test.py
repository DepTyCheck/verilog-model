from common.test_runner import run_test_suite
from regression_test.tests.test_error_checker import TestIterRegressionInputs, TestLoadAllErrorFiles
from regression_test.tests.test_error_file_parser import TestErrorFileParser
from regression_test.tests.test_result_reporter import (TestBuildReproducibilityReport, TestFormatMarkdownTable, TestReportForTwoExamplesWithFull,
                                                        TestSaveReport)

if __name__ == "__main__":
    run_test_suite(
        [
            TestErrorFileParser,
            TestIterRegressionInputs,
            TestLoadAllErrorFiles,
            TestBuildReproducibilityReport,
            TestFormatMarkdownTable,
            TestSaveReport,
            TestReportForTwoExamplesWithFull,
        ]
    )
