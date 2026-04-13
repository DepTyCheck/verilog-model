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

- Using iverilog
  From `ci/runner`:

```bash
pip install -r regression_test/requirements.txt && python -m regression_test.integration_tests.test_iverilog
```
