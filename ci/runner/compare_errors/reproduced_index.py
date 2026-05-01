from pathlib import Path

from common.error_file_parser import parse_error_files
from common.per_file_report import load_report
from regression_analyze.reproducibility import parse_example_filename


class ReproducedIndex:
    """For each known ``error_id``, did at least one regression example reproduce?

    Inputs:
      - ``regression_dir``: contains ``regression-<tool>-per-file.json`` files.
      - ``dataset_path``: clone of the dataset branch; YAMLs live under
        ``found_issues/<tool>/``.

    Behaviour: for each per-file JSON, infer the tool from ``tool_name``,
    parse the matching ``found_issues/<tool>/`` YAMLs to build a
    ``(example_name, type) -> error_id`` map, and walk the per-file report.
    A file is "reproduced" iff its filename parses to a known
    ``(example_name, type)`` and any match record on that file has
    ``error_id`` equal to the expected one.

    Unknown ``error_id`` lookups return ``False``.
    """

    def __init__(self, regression_dir: str | Path, dataset_path: str | Path):
        self._reproduced: dict[str, bool] = {}
        self._scan(Path(regression_dir), Path(dataset_path))

    def reproduced(self, error_id: str) -> bool:
        return self._reproduced.get(error_id, False)

    def _scan(self, regression_dir: Path, dataset_path: Path) -> None:
        if not regression_dir.exists():
            return
        for path in sorted(regression_dir.glob("regression-*-per-file.json")):
            self._ingest(path, dataset_path)

    def _ingest(self, path: Path, dataset_path: Path) -> None:
        report = load_report(path)
        tool = report.tool_name
        expected = self._build_expected(dataset_path, tool)
        for f in report.files:
            parsed = parse_example_filename(f.filename)
            if parsed is None:
                continue
            if parsed not in expected:
                continue
            error_id = expected[parsed]
            reproduced = any(m.error_id == error_id for c in f.commands for m in c.matches)
            self._reproduced[error_id] = self._reproduced.get(error_id, False) or reproduced

    @staticmethod
    def _build_expected(dataset_path: Path, tool: str) -> dict[tuple[str, str], str]:
        sub = dataset_path / "found_issues" / tool
        expected: dict[tuple[str, str], str] = {}
        if not sub.exists():
            return expected
        for ef in parse_error_files(str(sub), tool=tool):
            for ex in ef.examples:
                expected[(ex.name, ex.type)] = ef.error_id
        return expected
