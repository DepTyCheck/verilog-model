# ci/runner/regression_input/tests/test.py
from common.test_runner import run_test_suite
from regression_input.tests.test_main import TestRegressionInputMain

if __name__ == "__main__":
    run_test_suite([TestRegressionInputMain])
