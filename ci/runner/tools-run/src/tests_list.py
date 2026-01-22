import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator

from src.assets import Assets
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


class RunStatsCounter:
    def __init__(self):
        self.run_stats = {}

        self.run_stats["clean"] = 0
        self.run_stats["handled_errors"] = 0
        self.run_stats["failed"] = 0

    def resolve(self, main_success: bool, main_result: AnalyzisResult, sim_success: bool | None, sim_result: AnalyzisResult | None):
        both_commands_success = main_success and sim_success if sim_success is not None else main_success

        only_synth_no_unexpected_errors = (sim_result is None) and len(main_result.unexpected_errors) == 0
        synth_and_sim_no_unexpected = (sim_result is not None) and len(main_result.unexpected_errors) + len(sim_result.unexpected_errors) == 0

        if both_commands_success:
            self.run_stats["clean"] += 1
        elif only_synth_no_unexpected_errors or synth_and_sim_no_unexpected:
            self.run_stats["handled_errors"] += 1
        else:
            self.run_stats["failed"] += 1


class TestsList:
    def __init__(
        self,
        files: Iterator[Path],
        ignored_errors_list: IgnoredErrorsList,
        main_error_regex: ToolErrorRegex,
        sim_error_regex: ToolErrorRegex | None,
        raw_synth_cmd: str,
        raw_sim_cmd: str | None,
        assets: Assets,
    ):
        self.files = files
        self.ignored_errors_list = ignored_errors_list
        self.main_error_regex = main_error_regex
        self.sim_error_regex = sim_error_regex
        self.raw_synth_cmd = raw_synth_cmd
        self.raw_sim_cmd = raw_sim_cmd
        self.assets = assets

    def run_all(self) -> TestsRunResult:
        run_stats_counter = RunStatsCounter()
        matches: list[ErrorMatchInTest] = []
        unexpected_errors: list[UnexpectedError] = []

        for file_path in self.files:
            file_path_str = str(file_path.resolve())
            with open(file_path, "r", encoding="utf-8") as file:
                file_content = file.read()

            with tempfile.TemporaryDirectory(dir=Path.cwd()) as tmp_dir:
                self.assets.copy_to_tmp_dir(tmp_dir)

                synth_success, synth_result = self.run_single(
                    file_path_str=file_path_str,
                    file_content=file_content,
                    raw_cmd=self.raw_synth_cmd,
                    error_regex=self.main_error_regex,
                    cwd=tmp_dir,
                )
                matches.extend(synth_result.found_matches)
                unexpected_errors.extend(synth_result.unexpected_errors)

                sim_result = None
                sim_success = None
                if synth_success and self.raw_sim_cmd is not None:
                    sim_success, sim_result = self.run_single(
                        file_path_str=file_path_str,
                        file_content=file_content,
                        raw_cmd=self.raw_sim_cmd,
                        error_regex=self.sim_error_regex,
                        cwd=tmp_dir,
                    )
                    matches.extend(sim_result.found_matches)
                    unexpected_errors.extend(sim_result.unexpected_errors)

                run_stats_counter.resolve(
                    main_success=synth_success,
                    main_result=synth_result,
                    sim_success=sim_success,
                    sim_result=sim_result,
                )

        return TestsRunResult(
            run_stats=run_stats_counter.run_stats,
            matches=matches,
            unexpected_errors=unexpected_errors,
        )

    def run_single(
        self,
        file_path_str: str,
        file_content: str,
        raw_cmd: str,
        error_regex: ToolErrorRegex,
        cwd: str,
    ) -> tuple[bool, AnalyzisResult]:
        cmd = RunCommand(
            raw_str_cmd=raw_cmd,
            file_path=file_path_str,
            file_content=file_content,
            cwd=cwd,
        )
        cmd_result = cmd.execute()

        if cmd_result.result_code_is_ok:
            return True, AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)
        else:
            analyzis_result = cmd_result.output.analyze(
                ignored_errors_list=self.ignored_errors_list,
                tool_error_regex=error_regex,
                file_path=file_path_str,
            )
            return False, analyzis_result
