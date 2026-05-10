# ci/runner/regression_analyze/tests/test.py
from common.test_runner import run_test_suite
from regression_analyze.tests.test_reproducibility import (
    TestBuildReproducibilityTable,
    TestParseExampleFilename,
)

if __name__ == "__main__":
    run_test_suite([TestParseExampleFilename, TestBuildReproducibilityTable])
