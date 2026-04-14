from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Iterable, Protocol

from common.command_config import CommandConfig
from common.command_output import AnalyzisResult
from common.single_file_runner import run_file
from tools_run.src.assets import Assets
from tools_run.src.ignored_errors_list import IgnoredErrorsList


@dataclass
class FileInput:
    content: str
    file_suffix: str
    context: Any  # opaque per-use-case metadata
    assets: Assets | None = None
    logical_name: str | None = None  # shown in error records; falls back to temp path


class ResultHandler(Protocol):
    def handle(self, file_input: FileInput, result: AnalyzisResult) -> None: ...


class ResultCollector:
    """Standard ResultHandler: accumulates (FileInput, AnalyzisResult) pairs."""

    def __init__(self) -> None:
        self._results: list[tuple[FileInput, AnalyzisResult]] = []

    def handle(self, file_input: FileInput, result: AnalyzisResult) -> None:
        self._results.append((file_input, result))

    def results(self) -> list[tuple[FileInput, AnalyzisResult]]:
        return list(self._results)

    def has_unknown_errors(self) -> bool:
        return any(r.unexpected_errors for _, r in self._results)


def run_all(
    inputs: Iterable[FileInput],
    commands: list[CommandConfig],
    known_errors: IgnoredErrorsList,
    handler: ResultHandler,
) -> None:
    """Stateless runner: iterate inputs, analyse each with run_file, dispatch to handler."""
    for file_input in inputs:
        result = run_file(
            content=file_input.content,
            commands=commands,
            known_errors=known_errors,
            file_suffix=file_input.file_suffix,
            assets=file_input.assets,
            logical_name=file_input.logical_name,
        )
        handler.handle(file_input, result)
