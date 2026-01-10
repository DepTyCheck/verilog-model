import re

from src.command_output import CommandOutput


def make_command(cmd: str, file_path: str, file_content: str) -> str:
    """
    Construct a command string to run a tool.

    Args:
        cmd (str): The complete command string (binary + options)
        file_path (str): Path to the file to process
        file_content (str): Content of the file (used to find top module)

    Returns:
        str: The complete command string ready for execution
    """
    command = cmd
    if "{top_module}" in command:
        command = command.replace("{top_module}", find_top(file_content))
    command = command.replace("{file}", file_path)
    return command


def find_top(file_content: str) -> str:
    """
    Find the top module name in the given file content.

    Args:
        file_content (str): The content of the file to search for the top module

    Returns:
        str: The name of the top module, or None if no top module is found
    """
    matches = re.findall(r"(?<=module )[A-z]+", file_content, re.MULTILINE)
    if matches:
        return matches[-1]

    raise Exception("No top module found")


class RunCommand:
    def __init__(self, raw_str_cmd: str, file_path: str, file_content: str):
        self.cmd = self.make_command(raw_str_cmd, file_path, file_content)

    def execute(self) -> tuple[bool, CommandOutput]:
        pass
