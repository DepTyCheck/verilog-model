from dataclasses import dataclass
from typing import Iterator
from pathlib import Path

from src.ignored_errors_list import IgnoredErrorsList
from src.tool_error_regex import ToolErrorRegex
from src.run_command import RunCommand
from src.command_output import AnalyzisResult
from src.error_match_in_test import ErrorMatchInTest
from src.handle_errors import UnexpectedErrorText


@dataclass
class TestsRunResult:
    run_stats: dict
    matches: list[ErrorMatchInTest]
    unexpected_errors: list[UnexpectedErrorText]


class TestsList:
    def __init__(
        self,
        files: Iterator[Path],
        ignored_errors_list: IgnoredErrorsList,
        tool_error_regex: ToolErrorRegex,
        raw_synth_cmd: str,
        raw_sim_cmd: str | None,
    ):
        self.test_file_paths = files
        self.ignored_errors_list = ignored_errors_list
        self.tool_error_regex = tool_error_regex
        self.raw_synth_cmd = raw_synth_cmd
        self.raw_sim_cmd = raw_sim_cmd

    def run_all(self) -> TestsRunResult:
        run_stats = {}
        matches: list[ErrorMatchInTest] = []
        unexpected_errors: list[UnexpectedErrorText] = []

        run_stats["clean"] = 0
        run_stats["handled_errors"] = 0
        run_stats["failed"] = 0

        for file_path in self.files:
            file_path_str = str(file_path)
            with open(file_path, "r", encoding="utf-8") as file:
                file_content = file.read()

            synth_success, synth_result = self.run_single(
                file_path_str=file_path_str,
                file_content=file_content,
                raw_cmd=self.raw_synth_cmd,
            )
            matches.extend(synth_result.matches)
            unexpected_errors.extend(synth_result.unexpected_errors)

            if synth_success:
                sim_result, sim_result = self.run_single(
                    file_path_str=file_path_str,
                    file_content=file_content,
                    raw_cmd=self.raw_synth_cmd,
                )
                matches.extend(sim_result.matches)
                unexpected_errors.extend(sim_result.unexpected_errors)

            if synth_success and sim_result:
                run_stats["clean"] += 1
            elif len(synth_result.unexpected_errors) + len(sim_result.unexpected_errors) == 0:
                run_stats["handled_errors"] += 1
            else:
                run_stats["failed"] += 1

        return run_stats, matches, unexpected_errors

    def run_single(self, file_path_str: str, file_content: str, raw_cmd: str) -> tuple[bool, AnalyzisResult]:
        cmd = RunCommand(
            raw_str_cmd=raw_cmd,
            file_path=file_path_str,
            file_content=file_content,
        )
        cmd_success, cmd_output = cmd.execute()

        cmd_result = cmd_output.analyze(
            ignored_errors_list=self.ignored_errors_list,
            tool_error_regex=self.tool_error_regex,
        )

        return cmd_success, cmd_result
