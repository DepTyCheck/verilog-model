#!/usr/bin/env bash
# Run all Python unit test suites in ci/runner.
# Discovers suites automatically: any subdirectory with tests/test.py is a suite.
# Activate your venv before running this script.
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

PASS=0
FAIL=0

for dir in */; do
  dir="${dir%/}"
  [[ -f "$dir/tests/test.py" ]] || continue

  echo "──────────────────────────────────────────"
  echo "  Suite: $dir"
  echo "──────────────────────────────────────────"

  if [[ -f "$dir/requirements.txt" ]]; then
    echo "  Installing $dir/requirements.txt …"
    pip install -q -r "$dir/requirements.txt"
  fi

  if python -m "$dir.tests.test"; then
    PASS=$((PASS + 1))
  else
    echo "  FAILED: $dir" >&2
    FAIL=$((FAIL + 1))
  fi
  echo ""
done

echo "══════════════════════════════════════════"
echo "  Results: $PASS passed, $FAIL failed"
echo "══════════════════════════════════════════"

[[ $FAIL -eq 0 ]]
