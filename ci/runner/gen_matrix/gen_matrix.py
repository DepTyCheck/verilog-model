"""Load tools.yaml and build GHA matrix structures."""

import yaml


def load_tools(tools_yaml_path: str) -> list[dict]:
    with open(tools_yaml_path) as f:
        data = yaml.safe_load(f)
    return data.get("tools", [])


def build_matrix(tools: list[dict]) -> dict:
    """Wrap each tool entry under a 'tool' key to match matrix.tool.* access in GHA."""
    return {"include": [{"tool": tool} for tool in tools]}
