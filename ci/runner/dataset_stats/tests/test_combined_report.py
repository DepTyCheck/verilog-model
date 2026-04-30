import csv
import os
import tempfile
import unittest

from dataset_stats.combined_report import CombinedReport
from dataset_stats.files_index import FilesIndex
from dataset_stats.first_found_index import FirstFoundIndex
from dataset_stats.issues_index import IssuesIndex
from dataset_stats.legacy_index import LegacyIndex

DATA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "data")
FOUND_OK = os.path.join(DATA_DIR, "found_issues")
FILES_OK = os.path.join(DATA_DIR, "files")
ISSUES = os.path.join(DATA_DIR, "issues.csv")
ISSUES_UNKNOWN = os.path.join(DATA_DIR, "issues_unknown_id.csv")
LEGACY = os.path.join(DATA_DIR, "legacy_stats.csv")


def _build_default():
    return CombinedReport.build(
        issues=IssuesIndex(ISSUES),
        files=FilesIndex(FILES_OK),
        first_found=FirstFoundIndex(FOUND_OK),
        legacy=LegacyIndex(LEGACY),
    )


class TestCombinedReportBuild(unittest.TestCase):
    def setUp(self):
        self.rows = {r.error_id: r for r in _build_default().rows}

    def test_alpha_sums_legacy_and_new(self):
        # legacy: runs=100, overall=50, test_files=40, last=2025-06-15
        # new from issues.csv: 4 rows, 2 distinct filenames, last=2025-08-01
        # window for new: first_found=2025-07-19, last=2025-08-01 -> 2 files in files/
        r = self.rows["alpha"]
        self.assertEqual(r.runs_for_that_issue, 100 + 2)
        self.assertEqual(r.overall_found_count, 50 + 4)
        self.assertEqual(r.test_files_count, 40 + 2)
        self.assertEqual(r.last_occurrence_date, "2025-08-01")
        self.assertEqual(r.last_occurrence_tool_commit, "tcB")
        self.assertEqual(r.last_model_commit, "mc2")

    def test_beta_legacy_date_wins(self):
        # legacy.last=2025-12-01, new.last=2026-01-01 -> new wins
        # first_found=2025-03-04, last=2026-01-01 -> 3 files in window (07-19, 08-01, 01-01)
        r = self.rows["beta"]
        self.assertEqual(r.runs_for_that_issue, 200 + 3)
        self.assertEqual(r.overall_found_count, 100 + 1)
        self.assertEqual(r.test_files_count, 80 + 1)
        self.assertEqual(r.last_occurrence_date, "2026-01-01")
        self.assertEqual(r.last_occurrence_tool_commit, "tcA")
        self.assertEqual(r.last_model_commit, "mc3")

    def test_zeta_legacy_only(self):
        r = self.rows["zeta"]
        self.assertEqual(r.runs_for_that_issue, 50)
        self.assertEqual(r.overall_found_count, 25)
        self.assertEqual(r.test_files_count, 10)
        self.assertEqual(r.last_occurrence_tool_commit, "legacyZ")
        self.assertEqual(r.last_occurrence_date, "2025-04-04")
        self.assertEqual(r.last_model_commit, "")

    def test_delta_new_only(self):
        # delta has no legacy entry. first_found=2026-02-15, last=2026-02-15 -> 1 file
        r = self.rows["delta"]
        self.assertEqual(r.runs_for_that_issue, 1)
        self.assertEqual(r.overall_found_count, 1)
        self.assertEqual(r.test_files_count, 1)
        self.assertEqual(r.last_occurrence_date, "2026-02-15")
        self.assertEqual(r.last_occurrence_tool_commit, "tcA")
        self.assertEqual(r.last_model_commit, "mc4")

    def test_sort_overall_desc(self):
        report = _build_default()
        ids_in_order = [r.error_id for r in report.rows]
        # overall_found_counts: beta=101, alpha=54, zeta=25, delta=1 -> beta, alpha, zeta, delta
        self.assertEqual(ids_in_order, ["beta", "alpha", "zeta", "delta"])


class TestCombinedReportErrors(unittest.TestCase):
    def test_unknown_error_id_raises(self):
        with self.assertRaises(KeyError):
            CombinedReport.build(
                issues=IssuesIndex(ISSUES_UNKNOWN),
                files=FilesIndex(FILES_OK),
                first_found=FirstFoundIndex(FOUND_OK),
                legacy=LegacyIndex(LEGACY),
            )


class TestCombinedReportZeroRuns(unittest.TestCase):
    """An error_id present only in legacy with runs=0 makes no sense, so we
    fabricate a scenario via IssuesIndex with first_found > files in window
    AND zero legacy runs by writing a custom legacy fixture inline."""

    def test_zero_combined_runs_raises(self):
        # Build everything fresh in a tmp dir to control inputs precisely.
        with tempfile.TemporaryDirectory() as td:
            issues_path = os.path.join(td, "issues.csv")
            with open(issues_path, "w", encoding="utf-8") as f:
                # alpha row at 2025-08-01 (matches happy fixture)
                f.write('"2025-08-01","tc","alpha","mc","2025_08_01-seed_3_4.sv"\n')

            files_dir = os.path.join(td, "files")
            os.makedirs(files_dir)
            # Empty files dir -> 0 files in any window

            legacy_path = os.path.join(td, "legacy_stats.csv")
            with open(legacy_path, "w", encoding="utf-8") as f:
                f.write(
                    "error_id,runs_for_that_issue,overall_found_count,test_files_count,"
                    "last_occurrence_tool_commit,last_occurrence_date\n"
                )
                # zero legacy runs
                f.write('alpha,0,1,1,legacyA,2025-06-15\n')

            with self.assertRaises(ValueError) as ctx:
                CombinedReport.build(
                    issues=IssuesIndex(issues_path),
                    files=FilesIndex(files_dir),
                    first_found=FirstFoundIndex(FOUND_OK),
                    legacy=LegacyIndex(legacy_path),
                )
            self.assertIn("zero runs", str(ctx.exception))


class TestCombinedReportCsv(unittest.TestCase):
    def test_save_csv_header_and_first_row(self):
        report = _build_default()
        with tempfile.NamedTemporaryFile("r+", suffix=".csv", delete=False, encoding="utf-8") as tmp:
            report.save_csv(tmp.name)
            tmp.seek(0)
            reader = list(csv.reader(tmp))
        self.assertEqual(
            reader[0],
            [
                "error_id",
                "runs_for_that_issue",
                "overall_found_count",
                "test_files_count",
                "last_occurrence_tool_commit",
                "last_occurrence_date",
                "last_model_commit",
            ],
        )
        # First data row = beta (highest overall = 101)
        self.assertEqual(reader[1][0], "beta")
        self.assertEqual(reader[1][2], "101")


class TestCombinedReportLastOccurrenceTiebreak(unittest.TestCase):
    """Cover both branches of the last-occurrence date comparison.

    Existing happy-path fixtures only exercise the new>legacy case. These tests
    cover new<legacy (legacy wins, model forced empty) and new==legacy (tie
    goes to new because the comparison is >=).
    """

    def _build(self, td: str, *, new_date: str, legacy_date: str) -> CombinedReport:
        """Build a CombinedReport for a single error_id 'gamma' parameterized
        by the dataset-era last date and the legacy-era last date.
        """
        # found_issues/tool-a/gamma.yaml: first_found 01.01.2025
        found = os.path.join(td, "found_issues", "tool-a")
        os.makedirs(found)
        with open(os.path.join(found, "gamma.yaml"), "w", encoding="utf-8") as f:
            f.write(
                "id: gamma\n"
                "title: Gamma\n"
                "examples:\n"
                "  - gamma_ex1:\n"
                "      first_found: 01.01.2025\n"
                "      minified_example: \"\"\n"
            )

        # files/ contains one file at new_date so new_runs == 1
        files_dir = os.path.join(td, "files")
        os.makedirs(files_dir)
        new_date_underscore = new_date.replace("-", "_")
        with open(
            os.path.join(files_dir, f"{new_date_underscore}-seed_1_2.sv"),
            "w",
            encoding="utf-8",
        ):
            pass

        # issues.csv: one row for gamma at new_date
        issues_path = os.path.join(td, "issues.csv")
        with open(issues_path, "w", encoding="utf-8") as f:
            f.write(
                f'"{new_date}","newTool","gamma","newModel",'
                f'"{new_date_underscore}-seed_1_2.sv"\n'
            )

        # legacy_stats.csv: gamma row at legacy_date with non-zero runs
        legacy_path = os.path.join(td, "legacy_stats.csv")
        with open(legacy_path, "w", encoding="utf-8") as f:
            f.write(
                "error_id,runs_for_that_issue,overall_found_count,test_files_count,"
                "last_occurrence_tool_commit,last_occurrence_date\n"
            )
            f.write(f"gamma,5,2,2,legacyTool,{legacy_date}\n")

        return CombinedReport.build(
            issues=IssuesIndex(issues_path),
            files=FilesIndex(files_dir),
            first_found=FirstFoundIndex(os.path.join(td, "found_issues")),
            legacy=LegacyIndex(legacy_path),
        )

    def test_legacy_date_wins_when_new_is_older(self):
        with tempfile.TemporaryDirectory() as td:
            report = self._build(td, new_date="2025-06-01", legacy_date="2026-01-01")
        row = report.rows[0]
        self.assertEqual(row.error_id, "gamma")
        self.assertEqual(row.last_occurrence_date, "2026-01-01")
        self.assertEqual(row.last_occurrence_tool_commit, "legacyTool")
        # When legacy wins, model_commit is forced empty per the design spec.
        self.assertEqual(row.last_model_commit, "")

    def test_new_wins_on_equal_date_tiebreak(self):
        with tempfile.TemporaryDirectory() as td:
            report = self._build(td, new_date="2026-03-15", legacy_date="2026-03-15")
        row = report.rows[0]
        self.assertEqual(row.error_id, "gamma")
        self.assertEqual(row.last_occurrence_date, "2026-03-15")
        self.assertEqual(row.last_occurrence_tool_commit, "newTool")
        self.assertEqual(row.last_model_commit, "newModel")


if __name__ == "__main__":
    unittest.main()
