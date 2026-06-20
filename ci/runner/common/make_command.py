import subprocess


def make_command(template: str, file_path: str, translate_hook: str) -> str:
    """
    Build a concrete command string by delegating to a per-profile hook.

    Runs ``python3 <translate_hook> <template> <file_path>`` and returns the
    hook's stdout (the concrete command) with trailing whitespace stripped. The
    hook owns all placeholder substitution ({file} and any profile-specific
    placeholders). Raises if the hook exits non-zero.
    """
    proc = subprocess.run(
        ["python3", translate_hook, template, file_path],
        capture_output=True,
        text=True,
        check=False,
    )
    if proc.returncode != 0:
        detail = proc.stderr.strip() or proc.stdout.strip()
        raise Exception(f"translate hook failed ({translate_hook}): {detail}")
    return proc.stdout.strip()
