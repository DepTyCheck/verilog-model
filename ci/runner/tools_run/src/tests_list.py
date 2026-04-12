import tempfile
from dataclasses import dataclass, field
from pathlib import Path

from common.command_config import CommandConfig
from common.command_output import AnalyzisResult, CommandOutput
from common.error_types import ErrorMatchInTest, UnexpectedError
from common.logger import get_logger
from common.make_command import make_command
from common.run_command import run_command
from tools_run.src.assets import Assets
from tools_run.src.ignored_errors_list import IgnoredErrorsList


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

    def resolve(
        self,
        successes: list[bool],
        results: list[AnalyzisResult],
    ):
        all_clean = all(successes)
        total_unexpected = sum(len(r.unexpected_errors) for r in results)

        if all_clean:
            self.run_stats["clean"] += 1
        elif total_unexpected == 0:
            self.run_stats["handled_errors"] += 1
        else:
            self.run_stats["failed"] += 1


class TestsList:
    def __init__(
        self,
        files: list[Path],
        ignored_errors_list: IgnoredErrorsList,
        commands: list[CommandConfig],
        assets: Assets,
    ):
        self.files = files
        self.ignored_errors_list = ignored_errors_list
        self.commands = commands
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

                successes: list[bool] = []
                results: list[AnalyzisResult] = []

                for cmd_config in self.commands:
                    success, result = self.run_single(
                        file_path_str=file_path_str,
                        file_content=file_content,
                        cmd_config=cmd_config,
                        cwd=tmp_dir,
                    )
                    successes.append(success)
                    results.append(result)
                    matches.extend(result.found_matches)
                    unexpected_errors.extend(result.unexpected_errors)

                    if not success:
                        break

                run_stats_counter.resolve(successes=successes, results=results)

                if any(r.unexpected_errors for r in results):
                    get_logger().info(f"{file_path_str} content:\n{file_content}")

        return TestsRunResult(
            run_stats=run_stats_counter.run_stats,
            matches=matches,
            unexpected_errors=unexpected_errors,
        )

    def run_single(
        self,
        file_path_str: str,
        file_content: str,
        cmd_config: CommandConfig,
        cwd: str,
    ) -> tuple[bool, AnalyzisResult]:
        cmd = make_command(cmd_config.run, file_path_str, file_content)
        cmd_result = run_command(cmd, cwd=cwd)

        if cmd_result.result_code_is_ok:
            return True, AnalyzisResult(found_matches=[], unexpected_errors=[], all_errors_are_known=True)

        if cmd_config.error_regex is None:
            excerpt = "\n".join(cmd_result.output.splitlines()[:3])
            return False, AnalyzisResult(
                found_matches=[],
                unexpected_errors=[UnexpectedError(tool_output_error_text=excerpt, test_file_path=file_path_str)],
                all_errors_are_known=False,
            )

        analyzis_result = CommandOutput(cmd_result.output).analyze(
            ignored_errors_list=self.ignored_errors_list,
            tool_error_regex=cmd_config.error_regex,
            file_path=file_path_str,
        )
        return False, analyzis_result
