from dataclasses import dataclass
from pathlib import Path

_PRINTER_PATTERNS = ["runner", "pretty", "print", "warnings", "uniquenames"]


def _is_printer_file(path: Path) -> bool:
    stem = path.stem.lower()
    return any(p in stem for p in _PRINTER_PATTERNS)


def _is_pure_code_line(line: str) -> bool:
    stripped = line.strip()
    if not stripped:
        return False
    if stripped.startswith("--"):
        return False
    if stripped.startswith("|||"):
        return False
    return True


@dataclass
class GroupCount:
    total: int
    pure: int


@dataclass
class LineCounts:
    printers: GroupCount
    specification: GroupCount

    @property
    def total_total(self) -> int:
        return self.printers.total + self.specification.total

    @property
    def total_pure(self) -> int:
        return self.printers.pure + self.specification.pure


def count_file(path: Path) -> tuple[int, int]:
    """Return (total_lines, pure_code_lines) for a single .idr file."""
    total = 0
    pure = 0
    with open(path, encoding="utf-8") as f:
        for line in f:
            total += 1
            if _is_pure_code_line(line):
                pure += 1
    return total, pure


def count_lines(root: Path) -> LineCounts:
    """Count lines in all .idr files under root, split into printers vs specification."""
    printer_total = printer_pure = 0
    spec_total = spec_pure = 0

    for idr_file in sorted(root.rglob("*.idr")):
        total, pure = count_file(idr_file)
        if _is_printer_file(idr_file):
            printer_total += total
            printer_pure += pure
        else:
            spec_total += total
            spec_pure += pure

    return LineCounts(
        printers=GroupCount(total=printer_total, pure=printer_pure),
        specification=GroupCount(total=spec_total, pure=spec_pure),
    )


def format_report(counts: LineCounts) -> str:
    lines = [
        "Total lines (including comments and documentation):",
        f"{counts.printers.total} printers + {counts.specification.total} specification = {counts.total_total} total",
        "",
        "Source lines of code:",
        f"{counts.printers.pure} printers + {counts.specification.pure} specification = {counts.total_pure} total",
    ]
    return "\n".join(lines)
