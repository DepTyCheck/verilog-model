# ci/runner/common/tests/test.py
from common.test_runner import run_test_suite
from common.tests.test_command_result import TestAnalyzeCommand, TestCommandResult
from common.tests.test_make_command import TestFindTopEntityVhdl, TestFindTopModuleSv, TestMakeCommand
from common.tests.test_per_file_report import TestFileOutcome, TestPerFileReportRoundtrip
from common.tests.test_single_file_runner import TestRunFileAssets, TestRunFileClean, TestRunFileFailure
from common.tests.test_tool_matrix_runner import TestFileInput, TestResultCollector, TestRunAll

if __name__ == "__main__":
    run_test_suite(
        [
            TestCommandResult,
            TestAnalyzeCommand,
            TestFindTopEntityVhdl,
            TestFindTopModuleSv,
            TestMakeCommand,
            TestRunFileClean,
            TestRunFileFailure,
            TestRunFileAssets,
            TestFileInput,
            TestResultCollector,
            TestRunAll,
            TestFileOutcome,
            TestPerFileReportRoundtrip,
        ]
    )
