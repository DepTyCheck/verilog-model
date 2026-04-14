from pathlib import Path
from typing import Iterable

from common.tool_matrix_runner import FileInput
from tools_run.src.assets import Assets


def iter_test_files(
    files: Iterable[Path],
    file_suffix: str,
    assets: Assets | None,
) -> Iterable[FileInput]:
    """Yield a FileInput for each file path, reading content from disk."""
    for path in files:
        yield FileInput(
            content=path.read_text(encoding="utf-8"),
            file_suffix=file_suffix,
            context=path,
            assets=assets,
            logical_name=str(path.resolve()),
        )
