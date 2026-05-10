# ci/runner/mds_report/unknown_error_entry.py
from dataclasses import dataclass


@dataclass
class UnknownErrorEntry:
    file_path: str
    error_text: str
