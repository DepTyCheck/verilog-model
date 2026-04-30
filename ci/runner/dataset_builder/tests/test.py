from common.test_runner import run_test_suite
from dataset_builder.tests.test_csv_writer import TestAppendRows
from dataset_builder.tests.test_filter import TestPassingFilenames
from dataset_builder.tests.test_per_file_report import TestLoadReport
from dataset_builder.tests.test_rename import TestRenameFile

if __name__ == "__main__":
    run_test_suite([TestRenameFile, TestLoadReport, TestPassingFilenames, TestAppendRows])
