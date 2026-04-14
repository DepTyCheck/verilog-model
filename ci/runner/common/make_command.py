import re


def find_top_module_sv(file_content: str) -> str:
    """Find the top (last) module name in the given SystemVerilog design."""
    matches = re.findall(r"(?<=module )[A-z]+", file_content, re.MULTILINE)
    if matches:
        return matches[-1]
    raise Exception("No top module found")


def find_top_entity_vhdl(file_content: str) -> str:
    """Find the top (last) entity name in the given VHDL design."""
    matches = re.findall(r"(?<=entity )[A-z]+(?= is)", file_content, re.MULTILINE | re.IGNORECASE)
    if matches:
        return matches[-1]
    raise Exception("No top entity found")


def make_command(cmd: str, file_path: str, file_content: str) -> str:
    """
    Build a concrete command string from a template.

    Placeholders: {file}, {top_module}, {vhdl_top_entity}
    """
    command = cmd
    if "{top_module}" in command:
        command = command.replace("{top_module}", find_top_module_sv(file_content))
    if "{vhdl_top_entity}" in command:
        command = command.replace("{vhdl_top_entity}", find_top_entity_vhdl(file_content))
    command = command.replace("{file}", file_path)
    return command
