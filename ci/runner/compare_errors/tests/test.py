from common.test_runner import run_test_suite

from .test_compare_errors import TestErrorPercentageDelta, TestErrorsComparison
from .test_current_index import TestCurrentIndex
from .test_historical_index import TestHistoricalIndex
from .test_reproduced_index import TestReproducedIndex
from .test_table_formatter import TestTableFormatter

if __name__ == "__main__":
    run_test_suite(
        [
            TestErrorPercentageDelta,
            TestErrorsComparison,
            TestTableFormatter,
            TestHistoricalIndex,
            TestCurrentIndex,
            TestReproducedIndex,
        ]
    )
