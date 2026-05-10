# compare_errors

Renders a Markdown table that compares historical occurrence percentages of
each known `error_id` against the current PR's run, plus a "Reproduced"
column derived from the regression-test artifacts.

## Inputs

- **Dataset branch clone** (`--dataset-path`): provides `issues.csv`,
  `files/`, `legacy_stats.csv`, and `found_issues/`. Aggregated by
  `dataset_stats.CombinedReport` into the historical denominator and count
  per `error_id`.
- **Per-file artifacts** (`--per-file-results-dir`): `<tool>-per-file.json`
  files uploaded by the `run-tools` job. Each match record under
  `files[].commands[].matches[]` increments the per-`error_id` count for
  the owning tool. The sentinel `error_id == "unknown"` is skipped.
- **Regression artifacts** (`--regression-results-dir`):
  `regression-<tool>-per-file.json` files uploaded by `regression-test`.
  Reproduced iff any regression example for an `error_id` had a matching
  match record.

## Output

Markdown table with columns:

| Error ID | Historical % | Current % | Delta % | Reproduced |
| -------- | -----------: | --------: | ------: | :--------- |

Sorted by `abs(delta)` descending. Always exits 0.

## Conventions

- `error_id` strings are unique per tool by project convention.
- Hits/files can exceed 100% because one file can yield multiple matches
  for the same `error_id`. Both historical and current sides follow this
  convention.
- Per-file artifacts and regression artifacts must be in **separate**
  directories. The CI workflow downloads everything with one glob and
  splits `regression-*-per-file.json` into a sibling dir.
- No tool may be named with a `regression-` prefix. The workflow's
  filename split relies on this prefix being unique to the regression-test
  artifacts.
