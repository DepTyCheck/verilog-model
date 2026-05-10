# ci/runner/runner/tests/test.py
from common.test_runner import run_test_suite
from runner.tests.test_main import TestRunnerMain

if __name__ == "__main__":
    run_test_suite([TestRunnerMain])
