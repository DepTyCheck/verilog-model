"""Tests for mds_report/mds_distances_report.py"""

import os
import tempfile
import unittest
from unittest.mock import MagicMock, patch

import numpy as np
from common.error_types import MatchingMode
from common.ignored_errors_list import IgnoredErrorsList
from mds_report.mds_distances_report import MDSDistancesReport
from mds_report.unknown_error_entry import UnknownErrorEntry


def _make_ignored(patterns: list[str]):
    return IgnoredErrorsList.from_patterns(patterns, MatchingMode.SPECIFIC)


class TestMDSDistancesReport(unittest.TestCase):

    def test_fewer_than_2_nodes_prints_message_no_file(self):
        report = MDSDistancesReport(
            new_errors=[UnknownErrorEntry("f.sv", "err")],
            ignored_errors=_make_ignored([]),  # 1 new + 0 known = 1 node < 2
            tool_name="test_tool",
            job_link="http://example.com",
        )
        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as f:
            out = f.name
        os.unlink(out)  # make sure it doesn't pre-exist
        report.save(out)
        self.assertFalse(os.path.exists(out))

    @patch("mds_report.mds_distances_report.MDS")
    @patch("mds_report.mds_distances_report.LZMANCD")
    def test_save_creates_html_file(self, mock_lzma, mock_mds):
        mock_lzma.return_value.distance.return_value = 0.5
        mock_mds_instance = MagicMock()
        mock_mds_instance.fit_transform.return_value = np.array([[0, 0], [1, 1], [2, 2]])
        mock_mds.return_value = mock_mds_instance

        report = MDSDistancesReport(
            new_errors=[
                UnknownErrorEntry("a.sv", "error A"),
                UnknownErrorEntry("b.sv", "error B"),
            ],
            ignored_errors=_make_ignored(["known pattern"]),
            tool_name="test_tool",
            job_link="http://example.com",
        )
        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as f:
            out = f.name
        try:
            report.save(out)
            self.assertTrue(os.path.exists(out))
            with open(out, encoding="utf-8") as fh:
                content = fh.read()
            self.assertGreater(len(content), 0)
        finally:
            if os.path.exists(out):
                os.unlink(out)


if __name__ == "__main__":
    unittest.main()
