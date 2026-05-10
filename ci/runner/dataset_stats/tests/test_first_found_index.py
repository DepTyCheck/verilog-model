import os
import unittest

from common.tests.first_found_index_cases import make_first_found_index_test_classes

TestFirstFoundIndexHappyPath, TestFirstFoundIndexErrors = make_first_found_index_test_classes(
    data_dir=os.path.join(os.path.dirname(os.path.abspath(__file__)), "data"),
    prefix="found_issues",
)

if __name__ == "__main__":
    unittest.main()
