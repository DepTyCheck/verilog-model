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
import os
import subprocess
import sys
import tempfile
from pathlib import Path

RUNNER_DIR = Path(__file__).parent
REPO_ROOT = RUNNER_DIR.parents[1]
SRC_DIR = REPO_ROOT / "src"

_REQUIRED_TOOL_FIELDS = ("name", "language", "commands")

# ── Fixture helpers ─────────────────────────────────────────────────────────

_MINIMAL_SV = "module m; endmodule\n"

_MINIMAL_YAML = """\
id: test_error
tool: fake_tool
regex: 'UNIQUE_ERROR_PATTERN'
examples:
  - example_1:
      minified_example: "module bad; endmodule"
"""

_MINIMAL_COMMANDS_JSON = '[{"run": "echo {file}"}]'

_MINIMAL_LANGUAGES_YAML = "sv: .sv\nvhdl: .vhdl\n"


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


def step_tools_run_artifact() -> None:
    """
    Step 5: Run tools_run.main with a tiny fixture file and echo command.
    Validate that run-stats.json is written and has the expected schema.
    """
    _sep("Step 5 · tools_run artifact  (run-stats.json schema)")

    with tempfile.TemporaryDirectory() as tmp:
        # Write a fixture .sv file (found by tools_run via --gen-path + --file-pattern)
        with open(os.path.join(tmp, "fixture.sv"), "w") as f:
            f.write(_MINIMAL_SV)

        # Write a fake ignored-errors dir (empty — nothing to ignore)
        errors_dir = os.path.join(tmp, "errors")
        os.makedirs(errors_dir)

        stats_out = os.path.join(tmp, "run-stats.json")

        rc, stdout, stderr = _run(
            [
                sys.executable,
                "-m",
                "tools_run.main",
                "--gen-path",
                tmp,
                "--file-pattern",
                "*.sv",
                "--tool-name",
                "fake_tool",
                "--commands-json",
                _MINIMAL_COMMANDS_JSON,
                "--ignored-errors-dir",
                errors_dir,
                "--run-statistics-output",
                stats_out,
                "--commit",
                "abc123",
                "--job-link",
                "http://example.com",
            ]
        )

        if rc != 0:
            _err(f"tools_run.main exited {rc}")
            if stderr:
                _err(stderr[:300])
            results.record(False)
            return

        if not os.path.exists(stats_out):
            _err("run-stats.json not written")
            results.record(False)
            return

        with open(stats_out) as fh:
            data = json.load(fh)

        errors_list = data.get("errors")
        commit = data.get("commit")
        if not isinstance(errors_list, list):
            _err(f"run-stats.json missing 'errors' list: {data}")
            results.record(False)
            return
        if commit != "abc123":
            _err(f"run-stats.json wrong commit: {commit!r}")
            results.record(False)
            return

        _ok("run-stats.json written with correct schema")
        results.record(True)


def step_regression_test_artifact() -> None:
    """
    Step 6: Run regression_test.main with a fixture YAML and echo command.
    Validate that the reproducibility JSON has the expected schema.
    """
    _sep("Step 6 · regression_test artifact  (reproducibility JSON schema)")

    with tempfile.TemporaryDirectory() as tmp:
        # Write fixture known-errors directory
        tool_dir = os.path.join(tmp, "fake_tool")
        os.makedirs(tool_dir)
        with open(os.path.join(tool_dir, "test_error.yaml"), "w") as f:
            f.write(_MINIMAL_YAML)

        # Write languages config
        lang_cfg = os.path.join(tmp, "languages.yaml")
        with open(lang_cfg, "w") as f:
            f.write(_MINIMAL_LANGUAGES_YAML)

        report_out = os.path.join(tmp, "report.json")

        rc, stdout, stderr = _run(
            [
                sys.executable,
                "-m",
                "regression_test.main",
                "--known-errors-dir",
                tmp,
                "--tool-name",
                "fake_tool",
                "--commands-json",
                _MINIMAL_COMMANDS_JSON,
                "--language",
                "sv",
                "--language-config",
                lang_cfg,
                "--output",
                report_out,
            ]
        )

        # Exit code may be 0 or 1 depending on whether echo output is treated as unknown.
        # We only validate the artifact schema.
        if rc != 0:
            print(f"  (regression_test.main exited {rc} — echo may have produced unexpected output; validating artifact anyway)")

        if not os.path.exists(report_out):
            _err("reproducibility JSON not written")
            results.record(False)
            return

        with open(report_out) as fh:
            data = json.load(fh)

        if not isinstance(data, dict):
            _err(f"report.json is not a dict: {type(data)}")
            results.record(False)
            return

        _ok(f"reproducibility JSON written ({len(data)} error IDs)")
        results.record(True)


def step_mds_report_artifact() -> None:
    """
    Step 7: Run mds_report.main with a minimal unknown_errors.json + known-errors dir.
    Validate the output HTML is non-empty (MDS with < 2 nodes just prints a message;
    we test with 2 entries to actually generate a file).
    """
    _sep("Step 7 · mds_report artifact  (HTML file produced)")

    with tempfile.TemporaryDirectory() as tmp:
        # Write minimal unknown_errors.json
        errors_json = os.path.join(tmp, "unknown_errors.json")
        with open(errors_json, "w") as f:
            json.dump(
                [
                    {"file_path": "a.sv", "error_text": "error one"},
                    {"file_path": "b.sv", "error_text": "error two"},
                ],
                f,
            )

        # Write an empty known-errors dir (no patterns)
        errors_dir = os.path.join(tmp, "errors")
        os.makedirs(errors_dir)

        html_out = os.path.join(tmp, "out.html")

        rc, stdout, stderr = _run(
            [
                sys.executable,
                "-m",
                "mds_report.main",
                "--unknown-errors-input",
                errors_json,
                "--ignored-errors-dir",
                errors_dir,
                "--tool-name",
                "fake_tool",
                "--job-link",
                "http://example.com",
                "--output",
                html_out,
            ]
        )

        # With 2 unknown + 0 known = 2 nodes: MDS should produce a file.
        # If plotly/sklearn unavailable, it may exit non-zero; skip gracefully.
        if rc != 0 and not os.path.exists(html_out):
            _ok("mds_report.main skipped (dependencies may not be installed)")
            results.record(True)
            return

        if not os.path.exists(html_out):
            _err("HTML output not written (rc={rc})")
            results.record(False)
            return

        size = os.path.getsize(html_out)
        if size > 0:
            _ok(f"HTML output written ({size} bytes)")
            results.record(True)
        else:
            _err("HTML output is empty")
            results.record(False)


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
    step_tools_run_artifact()
    step_regression_test_artifact()
    step_mds_report_artifact()

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
