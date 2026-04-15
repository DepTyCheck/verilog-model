import yaml


def load_language_config(path: str) -> dict[str, str]:
    """Load the language → file-extension map from a YAML file."""
    with open(path, encoding="utf-8") as f:
        return yaml.safe_load(f)


def get_file_extension(language: str, extensions: dict[str, str]) -> str:
    if language not in extensions:
        raise ValueError(f"Unknown language '{language}'. Available: {sorted(extensions)}")
    return extensions[language]
