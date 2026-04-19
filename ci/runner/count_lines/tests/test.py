from common.test_runner import run_test_suite

from .test_count_lines import TestCountFile, TestCountLines, TestFormatReport, TestIsPrinterFile, TestIsPureCodeLine

if __name__ == "__main__":
    run_test_suite(
        [
            TestIsPrinterFile,
            TestIsPureCodeLine,
            TestCountFile,
            TestCountLines,
            TestFormatReport,
        ]
    )
