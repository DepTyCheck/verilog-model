from datetime import date

from common.first_found_index import FirstFoundIndex
from dataset_stats.files_index import FilesIndex
from dataset_stats.issues_index import IssuesIndex


class MasterIndex:
    """Per-error_id occurrence % since master last changed.

    For each error the window is ``[max(master_commit_date, first_found), today]``.
    ``master_pct = occurrences_in_window / runs_in_window * 100``.

    Errors absent from issues.csv (legacy-only, or with a found_issues yaml but no
    runs yet) return 0.0 and never touch ``first_found``, so a missing yaml for
    such an error does not raise.
    """

    def __init__(
        self,
        issues: IssuesIndex,
        files: FilesIndex,
        first_found: FirstFoundIndex,
        master_commit_date: date,
        today: date,
    ):
        self._issues = issues
        self._files = files
        self._first_found = first_found
        self._master_date = master_commit_date
        self._today = today

    def error_ids(self) -> set[str]:
        return set(self._issues.error_ids())

    def master_pct(self, error_id: str) -> float:
        dates = self._issues.occurrence_dates(error_id)
        if not dates:
            return 0.0
        start = max(self._master_date, self._first_found.lookup(error_id))
        occ = sum(1 for d in dates if start <= d <= self._today)
        runs = self._files.count_in_window(start, self._today)
        if runs == 0:
            return 0.0
        return occ / runs * 100.0
