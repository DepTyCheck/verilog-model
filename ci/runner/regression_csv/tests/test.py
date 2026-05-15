# ci/runner/regression_csv/tests/test.py
from common.test_runner import run_test_suite
from regression_csv.tests.test_csv_writer import TestFormatCsv
from regression_csv.tests.test_extract import (
    TestExtractRowsFromReport,
    TestToolNameFromArtifact,
)
from regression_csv.tests.test_main import TestRegressionCsvMain
from regression_csv.tests.test_parse_args import TestParseArgs

if __name__ == "__main__":
    run_test_suite(
        [
            TestFormatCsv,
            TestToolNameFromArtifact,
            TestExtractRowsFromReport,
            TestParseArgs,
            TestRegressionCsvMain,
        ]
    )
