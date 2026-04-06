from common.test_runner import run_test_suite

from .test_compare_errors import TestErrorPercentageDelta, TestErrorsComparison
from .test_table_formatter import TestTableFormatter

if __name__ == "__main__":
    run_test_suite(
        [
            TestErrorPercentageDelta,
            TestErrorsComparison,
            TestTableFormatter,
        ]
    )
