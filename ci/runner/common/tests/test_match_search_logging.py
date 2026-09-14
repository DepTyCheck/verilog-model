import logging
import unittest

from common.error_types import MatchingMode
from common.ignored_errors_list import IgnoredErrorsList
from common.line_group_errors import carve_group


class TestMatchSearchLogging(unittest.TestCase):
    def test_match_logs_searching_haystack_multiline(self):
        haystack = "line one\nline two"
        lst = IgnoredErrorsList.from_patterns([r"never"], MatchingMode.SPECIFIC)
        with self.assertLogs("ci_runner", level="INFO") as cm:
            lst.match(haystack, MatchingMode.SPECIFIC)
        joined = "\n".join(cm.output)
        self.assertIn("searching a match for line one\nline two", joined)
        self.assertTrue(all(record.levelno == logging.INFO for record in cm.records))

    def test_carve_group_logs_searching_haystack(self):
        atoms = ["A", "B"]
        patterns = [("id", r"NOPE")]
        with self.assertLogs("ci_runner", level="INFO") as cm:
            carve_group(atoms, patterns)
        joined = "\n".join(cm.output)
        self.assertIn("searching a match for A\nB", joined)
        self.assertTrue(all(record.levelno == logging.INFO for record in cm.records))
