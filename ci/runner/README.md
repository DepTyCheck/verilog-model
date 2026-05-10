# For local debug purposes

- Activate python venv before running any tests

## Unit tests

```bash
sh local_run_unit_tests.sh
```

## Integration tests

- Local overall artifacts check.
  From `ci/runner`:

```bash
python local_integration_test.py
```

## Dev

Handle legacy stats.

```bash
PYTHONPATH=ci/runner python -m legacy_stats.main \
      --previous-report ./latest-error-stats.json \
      --found-errors-dir ./verilog-gh-pages/found_errors \
      --output ./legacy_stats.csv
```

Combine legacy stats with modern.

```bash
PYTHONPATH=ci/runner python -m dataset_stats.main \
    --issues-csv debug_dataset/issues.csv \
    --files-dir debug_dataset/files \
    --found-issues-dir debug_dataset/found_issues \
    --legacy-stats-csv debug_dataset/legacy_stats.csv \
    --output combined_stats.csv
```
