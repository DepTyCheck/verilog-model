from dataclasses import dataclass, field
from pathlib import Path
from typing import List

import yaml
from common.error_types import MatchingMode
from common.logger import get_logger


@dataclass
class Example:
    name: str
    type: str  # "minified" | "full"
    content: str


@dataclass
class ErrorFile:
    error_id: str
    tool: str
    regex: str
    mode: MatchingMode
    title: str
    language: str  # source language of examples ("sv", "vhdl", …); read from "profile" YAML key
    examples: List[Example] = field(default_factory=list)


def _parse_examples(raw_examples) -> List[Example]:
    """
    Parse the examples block from a YAML error file.

    Each item in raw_examples is a dict like:
        { "example_name": { "minified_example": "...", "full_example": "..." } }
    """
    examples: List[Example] = []
    if not raw_examples:
        return examples

    for entry in raw_examples:
        if not isinstance(entry, dict):
            continue
        for example_name, data in entry.items():
            if not isinstance(data, dict):
                continue
            minified = data.get("minified_example")
            if minified:
                examples.append(Example(name=example_name, type="minified", content=minified))
            full = data.get("full_example")
            if full:
                examples.append(Example(name=example_name, type="full", content=full))
    return examples


def parse_error_files(dir_path: str, tool: str | None = None) -> List[ErrorFile]:
    """
    Load all YAML error files from dir_path.

    If tool is given, only load files whose 'target' YAML key matches.
    Files missing 'id' or 'regex' are skipped with a warning.
    """
    path = Path(dir_path)
    if not path.exists():
        get_logger().warning(f"Error files directory does not exist: {path.absolute()}")
        return []

    yaml_files = list(path.glob("*.yaml"))
    if not yaml_files:
        get_logger().warning(f"No YAML files found in: {path.absolute()}")

    result: List[ErrorFile] = []
    for yaml_file in yaml_files:
        try:
            with open(yaml_file, "r", encoding="utf-8") as f:
                data = yaml.safe_load(f)

            file_tool = data.get("target")
            if tool is not None and file_tool != tool:
                continue

            error_id = data.get("id")
            regex = data.get("regex")
            if error_id is None or regex is None:
                get_logger().warning(f"Skipping {yaml_file}: missing 'id' or 'regex'")
                continue

            mode_raw = data.get("matching_mode")
            mode = MatchingMode.WHOLE if mode_raw == "whole" else MatchingMode.SPECIFIC

            result.append(
                ErrorFile(
                    error_id=error_id,
                    tool=file_tool or "",
                    regex=regex.rstrip("\n"),
                    mode=mode,
                    title=data.get("title", ""),
                    language=data.get("profile", "sv"),
                    examples=_parse_examples(data.get("examples")),
                )
            )
        except Exception as e:
            get_logger().warning(f"Failed to parse {yaml_file}: {e}")

    return result
