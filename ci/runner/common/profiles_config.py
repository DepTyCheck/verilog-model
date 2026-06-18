import yaml


def load_profiles_config(path: str) -> dict[str, dict]:
    """Load the profile -> options map (file_extension, translate_hook) from a YAML file."""
    with open(path, encoding="utf-8") as f:
        return yaml.safe_load(f)


def get_file_extension(profile: str, profiles: dict[str, dict]) -> str:
    if profile not in profiles:
        raise ValueError(f"Unknown profile '{profile}'. Available: {sorted(profiles)}")
    return profiles[profile]["file_extension"]
