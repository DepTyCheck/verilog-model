## Show example output

From `ci/runner`

```bash
python -m compare_errors.main \
  --previous-report compare_errors/tests/data/previous_report.json \
  --current-tools-reports-dir compare_errors/tests/data \
  --tools-reports-pattern "*-run-stats.json" \
  --tests-number 256 \
  --error-url-prefix "https://deptycheck.github.io/verilog-model/error" \
  --known-errors-reports-dir compare_errors/tests/data
```
