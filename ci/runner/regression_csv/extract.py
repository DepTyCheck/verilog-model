"""Pull `(example_id, type, reproduced)` rows out of one regression artifact.

Tool name is parsed from the artifact filename
(`regression-<tool>-per-file.json`). The expected map is built by callers via
`regression_analyze.expected.build_expected`.
"""

import re
from pathlib import Path
from typing import Iterator

from common.per_file_report import load_report
from regression_analyze.reproducibility import iter_reproductions

_ARTIFACT_RE = re.compile(r"^regression-(.+)-per-file\.json$")


def tool_name_from_artifact(filename: str) -> str | None:
    match = _ARTIFACT_RE.match(Path(filename).name)
    return match.group(1) if match else None


def extract_rows_from_report(
    artifact_path: Path,
    expected: dict[tuple[str, str], str],
) -> Iterator[tuple[str, str, bool]]:
    report = load_report(artifact_path)
    for (example_name, ex_type), _error_id, reproduced in iter_reproductions(report, expected):
        yield example_name, ex_type, reproduced
