from dataclasses import dataclass

from common.tool_error_regex import ToolErrorRegex


@dataclass
class CommandConfig:
    run: str
    error_regex: ToolErrorRegex | None = None
