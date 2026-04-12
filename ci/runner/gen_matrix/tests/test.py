from common.test_runner import run_test_suite
from gen_matrix.tests.test_gen_matrix import TestBuildMatrix, TestLoadTools, TestRoundTrip

if __name__ == "__main__":
    run_test_suite([TestLoadTools, TestBuildMatrix, TestRoundTrip])
