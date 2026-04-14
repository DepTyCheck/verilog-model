from common.test_runner import run_test_suite
from common.tests.test_single_file_runner import TestRunFileAssets, TestRunFileClean, TestRunFileFailure
from common.tests.test_tool_matrix_runner import TestFileInput, TestResultCollector, TestRunAll
from common.tests.test_unknown_error_reporter import TestCollectUnknownErrors, TestPrintUnknownErrors, TestSaveUnknownErrorsJson

if __name__ == "__main__":
    run_test_suite(
        [
            TestRunFileClean,
            TestRunFileFailure,
            TestRunFileAssets,
            TestFileInput,
            TestResultCollector,
            TestRunAll,
            TestCollectUnknownErrors,
            TestSaveUnknownErrorsJson,
            TestPrintUnknownErrors,
        ]
    )
