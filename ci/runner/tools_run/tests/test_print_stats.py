import unittest

from common.command_output import AnalyzisResult
from common.error_types import ErrorMatchInTest, FoundMatch, IgnoredError, UnexpectedError
from common.tool_matrix_runner import FileInput


def _fi() -> FileInput:
    return FileInput(content="", file_suffix=".sv", context=None)


def _clean() -> AnalyzisResult:
    return AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)


def _handled() -> AnalyzisResult:
    match = FoundMatch(error=IgnoredError(".*"), matched_text="x")
    return AnalyzisResult(
        found_matches=[ErrorMatchInTest(match=match, test_path="f.sv")],
        unexpected_errors=[],
        all_errors_are_known=True,
    )


def _failed() -> AnalyzisResult:
    return AnalyzisResult(
        found_matches=[],
        unexpected_errors=[UnexpectedError("boom", "f.sv")],
        all_errors_are_known=False,
    )


class TestCountRunStats(unittest.TestCase):

    def test_all_clean(self):
        from tools_run.src.print_stats import count_run_stats

        stats = count_run_stats([(_fi(), _clean()), (_fi(), _clean())])
        self.assertEqual(stats["clean"], 2)
        self.assertEqual(stats["handled_errors"], 0)
        self.assertEqual(stats["failed"], 0)

    def test_handled(self):
        from tools_run.src.print_stats import count_run_stats

        stats = count_run_stats([(_fi(), _handled())])
        self.assertEqual(stats["handled_errors"], 1)
        self.assertEqual(stats["clean"], 0)

    def test_failed(self):
        from tools_run.src.print_stats import count_run_stats

        stats = count_run_stats([(_fi(), _failed())])
        self.assertEqual(stats["failed"], 1)

    def test_mixed(self):
        from tools_run.src.print_stats import count_run_stats

        stats = count_run_stats([(_fi(), _clean()), (_fi(), _handled()), (_fi(), _failed())])
        self.assertEqual(stats, {"clean": 1, "handled_errors": 1, "failed": 1})


if __name__ == "__main__":
    unittest.main()
