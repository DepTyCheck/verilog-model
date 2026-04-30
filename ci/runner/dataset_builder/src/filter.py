from dataset_builder.src.per_file_report import ToolReport

_PASS_OUTCOMES = {"clean", "known_errors"}


def passing_filenames(reports: list[ToolReport]) -> set[str]:
    """Return filenames that have only clean/known_errors outcomes across all tool reports."""
    passes: dict[str, bool] = {}
    for report in reports:
        for file_record in report.files:
            fname = file_record.filename
            if fname not in passes:
                passes[fname] = True
            if file_record.outcome not in _PASS_OUTCOMES:
                passes[fname] = False
    return {fname for fname, ok in passes.items() if ok}
