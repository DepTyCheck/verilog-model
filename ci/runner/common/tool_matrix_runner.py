# ci/runner/common/tool_matrix_runner.py
from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Iterable, Protocol

from common.assets import Assets
from common.command_config import CommandConfig
from common.ignored_errors_list import IgnoredErrorsList
from common.run_tool_command import CommandResult
from common.single_file_runner import run_file


@dataclass
class FileInput:
    content: str
    file_suffix: str
    context: Any  # opaque per-use-case metadata
    assets: Assets | None = None
    logical_name: str | None = None  # shown in error records; falls back to temp path


class ResultHandler(Protocol):
    def handle(self, file_input: FileInput, results: list[CommandResult]) -> None: ...


class ResultCollector:
    """Standard ResultHandler: accumulates (FileInput, list[CommandResult]) pairs."""

    def __init__(self) -> None:
        self._results: list[tuple[FileInput, list[CommandResult]]] = []

    def handle(self, file_input: FileInput, results: list[CommandResult]) -> None:
        self._results.append((file_input, results))

    def results(self) -> list[tuple[FileInput, list[CommandResult]]]:
        return list(self._results)

    def has_unknown_errors(self) -> bool:
        return any(cr.outcome == "unknown" for _, results in self._results for cr in results)


def run_all(
    inputs: Iterable[FileInput],
    commands: list[CommandConfig],
    known_errors: IgnoredErrorsList,
    handler: ResultHandler,
) -> None:
    """Iterate inputs, run each through run_file, dispatch the per-command list to handler."""
    for file_input in inputs:
        results = run_file(
            content=file_input.content,
            commands=commands,
            known_errors=known_errors,
            file_suffix=file_input.file_suffix,
            assets=file_input.assets,
            logical_name=file_input.logical_name,
        )
        handler.handle(file_input, results)
