import json
from dataclasses import dataclass

from common.tool_error_regex import ToolErrorRegex


@dataclass
class CommandConfig:
    run: str
    error_regex: ToolErrorRegex | None = None


def parse_commands(commands_json: str) -> list["CommandConfig"]:
    raw = json.loads(commands_json)
    return [
        CommandConfig(
            run=entry["run"],
            error_regex=ToolErrorRegex(entry["error_regex"]) if entry.get("error_regex") else None,
        )
        for entry in raw
    ]
