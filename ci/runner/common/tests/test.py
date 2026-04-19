from common.test_runner import run_test_suite
from common.tests.test_make_command import TestFindTopEntityVhdl, TestFindTopModuleSv, TestMakeCommand
from common.tests.test_single_file_runner import TestRunFileAssets, TestRunFileClean, TestRunFileFailure
from common.tests.test_tool_matrix_runner import TestFileInput, TestResultCollector, TestRunAll
from common.tests.test_unknown_error_reporter import TestCollectUnknownErrors, TestPrintUnknownErrors, TestSaveUnknownErrorsJson

if __name__ == "__main__":
    run_test_suite(
        [
            TestFindTopEntityVhdl,
            TestFindTopModuleSv,
            TestMakeCommand,
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
