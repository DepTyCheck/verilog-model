# ci/runner/common/tests/test.py
from common.test_runner import run_test_suite
from common.tests.test_command_result import TestAnalyzeCommand, TestCommandResult
from common.tests.test_error_file_parser import TestProfileRequired
from common.tests.test_first_found_index import TestFirstFoundIndexErrors, TestFirstFoundIndexHappyPath
from common.tests.test_make_command import TestMakeCommand
from common.tests.test_per_file_report import TestFileOutcome, TestPerFileReportRoundtrip
from common.tests.test_profiles_config import TestProfilesConfig
from common.tests.test_resolve_profile import TestResolveProfile
from common.tests.test_single_file_runner import TestRunFileAssets, TestRunFileClean, TestRunFileFailure
from common.tests.test_tool_matrix_runner import TestFileInput, TestResultCollector, TestRunAll
from common.tests.test_translate_hooks import TestSvHook, TestVhdlHook

if __name__ == "__main__":
    run_test_suite(
        [
            TestCommandResult,
            TestAnalyzeCommand,
            TestMakeCommand,
            TestRunFileClean,
            TestRunFileFailure,
            TestRunFileAssets,
            TestFileInput,
            TestResultCollector,
            TestRunAll,
            TestFileOutcome,
            TestPerFileReportRoundtrip,
            TestFirstFoundIndexHappyPath,
            TestFirstFoundIndexErrors,
            TestSvHook,
            TestVhdlHook,
            TestProfileRequired,
            TestProfilesConfig,
            TestResolveProfile,
        ]
    )
