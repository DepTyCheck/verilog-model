"""Build the (example_name, type) -> error_id map for a single tool.

The map drives `regression_analyze.reproducibility.iter_reproductions`.
"""

from pathlib import Path

from common.error_file_parser import parse_error_files


def build_expected(known_errors_dir: str, tool_name: str) -> dict[tuple[str, str], str]:
    base = Path(known_errors_dir)
    expected: dict[tuple[str, str], str] = {}
    if not base.exists():
        return expected
    for sub in sorted(p for p in base.iterdir() if p.is_dir()):
        for ef in parse_error_files(str(sub)):
            if ef.tool != tool_name:
                continue
            for ex in ef.examples:
                expected[(ex.name, ex.type)] = ef.error_id
    return expected
