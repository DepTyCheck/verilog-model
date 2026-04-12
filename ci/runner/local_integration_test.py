#!/usr/bin/env python3
"""
Local CI integration test — mirrors the locally-runnable jobs in ci-package.yml
in the same order and validates each artifact before proceeding.

Pipeline:
  Step 1  get-tool-matrix   → gen_matrix.main        → matrix/check-matrix JSON
  Step 2  get-test-matrix   → gen_test_matrix.py     → suite discovery JSON
  Step 3  test-python-units → each suite from Step 2 → all suites must pass
  Step 4  count-lines       → count_lines.main        → non-empty text output

Steps 2 and 3 form a pipeline: Step 3 uses the suite list produced by Step 2.
If Step 1 or Step 2 fails the script aborts early (downstream can't run).

Exit code 0 only if all steps pass.

Run from anywhere — the script locates itself:
  python ci/runner/integration_test.py
or from ci/runner/:
  python integration_test.py
"""

import json
import subprocess
import sys
from pathlib import Path

RUNNER_DIR = Path(__file__).parent
REPO_ROOT = RUNNER_DIR.parents[1]
SRC_DIR = REPO_ROOT / "src"

_REQUIRED_TOOL_FIELDS = ("name", "language", "commands")


# ---------------------------------------------------------------------------
# Result tracking
# ---------------------------------------------------------------------------


class Results:
    def __init__(self) -> None:
        self.passed = 0
        self.failed = 0

    def record(self, ok: bool) -> bool:
        if ok:
            self.passed += 1
        else:
            self.failed += 1
        return ok

    @property
    def all_ok(self) -> bool:
        return self.failed == 0


results = Results()


# ---------------------------------------------------------------------------
# Output helpers
# ---------------------------------------------------------------------------


def _sep(title: str) -> None:
    print(f"\n{'─' * 64}")
    print(f"  {title}")
    print(f"{'─' * 64}")


def _ok(msg: str) -> None:
    print(f"  ✓  {msg}")


def _err(msg: str) -> None:
    print(f"  ✗  {msg}", file=sys.stderr)


# ---------------------------------------------------------------------------
# Subprocess helper
# ---------------------------------------------------------------------------


def _run(cmd: list[str]) -> tuple[int, str, str]:
    r = subprocess.run(cmd, capture_output=True, text=True, cwd=RUNNER_DIR)
    return r.returncode, r.stdout, r.stderr


def _parse_kv_output(stdout: str) -> dict[str, str]:
    """Parse key=value lines (mirrors $GITHUB_OUTPUT format)."""
    out: dict[str, str] = {}
    for line in stdout.splitlines():
        if "=" in line:
            k, _, v = line.partition("=")
            out[k.strip()] = v.strip()
    return out


# ---------------------------------------------------------------------------
# Artifact validators (pure functions — no side effects)
# ---------------------------------------------------------------------------


def _validate_tool_matrix(label: str, data: dict) -> list[str]:
    """Return list of error strings for a tool matrix dict."""
    errors: list[str] = []
    include = data.get("include")
    if not isinstance(include, list) or not include:
        errors.append(f"'{label}': 'include' must be a non-empty list")
        return errors

    for item in include:
        tool = item.get("tool")
        if not isinstance(tool, dict):
            errors.append(f"'{label}': entry missing 'tool' object: {item}")
            continue
        name = tool.get("name", "?")
        for field in _REQUIRED_TOOL_FIELDS:
            if field not in tool:
                errors.append(f"'{label}': tool '{name}' missing required field '{field}'")
        cmds = tool.get("commands")
        if isinstance(cmds, list):
            if not cmds:
                errors.append(f"'{label}': tool '{name}' has empty commands list")
            for cmd in cmds:
                if not isinstance(cmd, dict) or "run" not in cmd:
                    errors.append(f"'{label}': tool '{name}' command missing 'run' field")

    return errors


def _validate_suite_matrix(data: dict) -> list[str]:
    """Return list of error strings for a test-suite matrix dict."""
    errors: list[str] = []
    include = data.get("include")
    if not isinstance(include, list) or not include:
        errors.append("suite matrix 'include' must be a non-empty list")
        return errors

    for item in include:
        suite = item.get("suite")
        if not isinstance(suite, dict) or "name" not in suite:
            errors.append(f"suite entry missing 'name': {item}")
            continue
        if "requirements" in suite:
            req_path = RUNNER_DIR / suite["requirements"]
            if not req_path.exists():
                errors.append(f"suite '{suite['name']}' references non-existent {suite['requirements']}")
    return errors


# ---------------------------------------------------------------------------
# Pipeline steps
# ---------------------------------------------------------------------------


def step_get_tool_matrix() -> dict | None:
    """
    Run gen_matrix.main, parse its stdout, validate both matrices.
    Returns the parsed artifacts dict on success, None on failure.
    """
    _sep("Step 1 · get-tool-matrix  (gen_matrix.main)")

    rc, stdout, stderr = _run([sys.executable, "-m", "gen_matrix.main"])
    if rc != 0:
        _err(f"gen_matrix.main exited {rc}")
        if stderr:
            _err(stderr.strip())
        results.record(False)
        return None

    kv = _parse_kv_output(stdout)
    artifacts: dict[str, dict] = {}
    errors: list[str] = []

    for key in ("matrix", "check-matrix"):
        if key not in kv:
            errors.append(f"missing output key '{key}'")
            continue
        try:
            data = json.loads(kv[key])
        except json.JSONDecodeError as e:
            errors.append(f"'{key}' is not valid JSON: {e}")
            continue
        errors.extend(_validate_tool_matrix(key, data))
        artifacts[key] = data

    if errors:
        for e in errors:
            _err(e)
        results.record(False)
        return None

    matrix_names = sorted(item["tool"]["name"] for item in artifacts["matrix"]["include"])
    check_names = sorted(item["tool"]["name"] for item in artifacts["check-matrix"]["include"])
    _ok(f"matrix: {len(matrix_names)} tools — {', '.join(matrix_names)}")
    _ok(f"check-matrix: {len(check_names)} tools — {', '.join(check_names)}")
    results.record(True)
    return artifacts


def step_get_test_matrix() -> list[dict] | None:
    """
    Run gen_test_matrix.py, parse its stdout, validate the suite list.
    Returns the list of suite dicts on success, None on failure.
    """
    _sep("Step 2 · get-test-matrix  (gen_test_matrix.py)")

    rc, stdout, stderr = _run([sys.executable, "gen_test_matrix.py"])
    if rc != 0:
        _err(f"gen_test_matrix.py exited {rc}")
        if stderr:
            _err(stderr.strip())
        results.record(False)
        return None

    kv = _parse_kv_output(stdout)
    if "matrix" not in kv:
        _err("missing output key 'matrix'")
        results.record(False)
        return None

    try:
        data = json.loads(kv["matrix"])
    except json.JSONDecodeError as e:
        _err(f"'matrix' is not valid JSON: {e}")
        results.record(False)
        return None

    errors = _validate_suite_matrix(data)
    if errors:
        for e in errors:
            _err(e)
        results.record(False)
        return None

    suites = [item["suite"] for item in data["include"]]
    _ok(f"discovered {len(suites)} suites: {', '.join(s['name'] for s in suites)}")
    results.record(True)
    return suites


def step_test_python_units(suites: list[dict]) -> None:
    """Run each discovered suite and track pass/fail per suite."""
    _sep("Step 3 · test-python-units  (each discovered suite)")

    suite_pass = 0
    suite_fail = 0

    for suite in suites:
        name = suite["name"]
        print(f"\n  Suite: {name}")

        if suite.get("requirements"):
            req = RUNNER_DIR / suite["requirements"]
            pip_rc, _, pip_err = _run([sys.executable, "-m", "pip", "install", "-q", "-r", str(req)])
            if pip_rc != 0:
                _err(f"pip install failed:\n{pip_err.strip()}")
                suite_fail += 1
                continue

        rc, stdout, stderr = _run([sys.executable, "-m", f"{name}.tests.test"])
        if rc == 0:
            _ok("passed")
            suite_pass += 1
        else:
            _err(f"FAILED (exit {rc})")
            if stdout:
                print(stdout, end="")
            if stderr:
                print(stderr, end="", file=sys.stderr)
            suite_fail += 1

    print()
    if suite_fail == 0:
        _ok(f"all {suite_pass} suite(s) passed")
        results.record(True)
    else:
        _err(f"{suite_fail} suite(s) failed, {suite_pass} passed")
        results.record(False)


def step_count_lines() -> None:
    """Run count_lines.main against src/; validate it produces output."""
    _sep("Step 4 · count-lines  (count_lines.main)")

    if not SRC_DIR.exists():
        _ok(f"skipped — src/ not found at {SRC_DIR}")
        results.record(True)
        return

    rc, stdout, stderr = _run([sys.executable, "-m", "count_lines.main", str(SRC_DIR)])
    if rc != 0:
        _err(f"count_lines.main exited {rc}")
        if stderr:
            _err(stderr.strip())
        results.record(False)
        return

    lines = stdout.strip().splitlines()
    if not lines:
        _err("count_lines.main produced no output")
        results.record(False)
        return

    _ok(f"produced {len(lines)} output line(s)")
    _ok(f"sample: {lines[0][:80]}")
    results.record(True)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main() -> None:
    print("CI Integration Test")
    print(f"Runner dir : {RUNNER_DIR}")
    print(f"Repo root  : {REPO_ROOT}")

    # Steps 1 and 2 produce artifacts consumed by downstream steps.
    # Abort early if either fails so we don't run tests against a bad matrix.
    tool_artifacts = step_get_tool_matrix()
    if tool_artifacts is None:
        _abort("tool matrix unavailable — cannot continue")

    suites = step_get_test_matrix()
    if suites is None:
        _abort("suite matrix unavailable — cannot run unit tests")

    step_test_python_units(suites)
    step_count_lines()

    _sep(f"Results: {results.passed} passed, {results.failed} failed")
    if results.all_ok:
        print("  All steps passed.")
    else:
        print("  INTEGRATION TEST FAILED", file=sys.stderr)

    sys.exit(0 if results.all_ok else 1)


def _abort(reason: str) -> None:
    _sep(f"ABORTED — {reason}")
    _sep(f"Results: {results.passed} passed, {results.failed} failed")
    sys.exit(1)


if __name__ == "__main__":
    main()
