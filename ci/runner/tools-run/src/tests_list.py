from dataclasses import dataclass
from pathlib import Path
from typing import Iterator

from src.command_output import AnalyzisResult
from src.error_match_in_test import ErrorMatchInTest
from src.ignored_errors_list import IgnoredErrorsList
from src.run_command import RunCommand
from src.tool_error_regex import ToolErrorRegex
from src.unexpected_error import UnexpectedError


@dataclass
class TestsRunResult:
    run_stats: dict
    matches: list[ErrorMatchInTest]
    unexpected_errors: list[UnexpectedError]

    def has_unexpected_errors(self) -> bool:
        return len(self.unexpected_errors) > 0


class TestsList:
    def __init__(
        self,
        files: Iterator[Path],
        ignored_errors_list: IgnoredErrorsList,
        tool_error_regex: ToolErrorRegex,
        raw_synth_cmd: str,
        raw_sim_cmd: str | None,
    ):
        self.files = files
        self.ignored_errors_list = ignored_errors_list
        self.tool_error_regex = tool_error_regex
        self.raw_synth_cmd = raw_synth_cmd
        self.raw_sim_cmd = raw_sim_cmd

    def run_all(self) -> TestsRunResult:
        run_stats = {}
        matches: list[ErrorMatchInTest] = []
        unexpected_errors: list[UnexpectedError] = []

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
            matches.extend(synth_result.found_matches)
            unexpected_errors.extend(synth_result.unexpected_errors)

            sim_result = None
            if synth_success:
                print("Run sim")
                sim_success, sim_result = self.run_single(
                    file_path_str=file_path_str,
                    file_content=file_content,
                    raw_cmd=self.raw_sim_cmd,
                )
                matches.extend(sim_result.found_matches)
                unexpected_errors.extend(sim_result.unexpected_errors)

            only_synth_no_unexpected_errors = (sim_result is None) and len(synth_result.unexpected_errors) == 0
            synth_and_sim_no_unexpected = (sim_result is not None) and len(synth_result.unexpected_errors) + len(sim_result.unexpected_errors) == 0

            if synth_success and sim_success:
                run_stats["clean"] += 1
            elif only_synth_no_unexpected_errors or synth_and_sim_no_unexpected:
                run_stats["handled_errors"] += 1
            else:
                run_stats["failed"] += 1

        return TestsRunResult(
            run_stats=run_stats,
            matches=matches,
            unexpected_errors=unexpected_errors,
        )

    def run_single(self, file_path_str: str, file_content: str, raw_cmd: str) -> tuple[bool, AnalyzisResult]:
        cmd = RunCommand(
            raw_str_cmd=raw_cmd,
            file_path=file_path_str,
            file_content=file_content,
        )
        cmd_result = cmd.execute()

        if cmd_result.result_code_is_ok:
            return True, AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)
        else:
            analyzis_result = cmd_result.output.analyze(
                ignored_errors_list=self.ignored_errors_list,
                tool_error_regex=self.tool_error_regex,
                file_path=file_path_str,
            )
            return False, analyzis_result
