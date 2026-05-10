import re

_SEED_RE = re.compile(r"^\d+-seed_(\d+),(\d+)(\.\w+)$")


def rename_file(original_name: str, run_date: str) -> str:
    """Transform '{N}-seed_{s1},{s2}.{ext}' -> '{run_date}-seed_{s1}_{s2}.{ext}'.

    run_date format: 'YYYY_MM_dd'
    Raises ValueError if original_name doesn't match the expected pattern.
    """
    m = _SEED_RE.match(original_name)
    if not m:
        raise ValueError(f"Filename does not match expected pattern: {original_name!r}")
    s1, s2, ext = m.group(1), m.group(2), m.group(3)
    return f"{run_date}-seed_{s1}_{s2}{ext}"
