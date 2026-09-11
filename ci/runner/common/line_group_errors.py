from __future__ import annotations

import re
from dataclasses import dataclass


@dataclass
class AtomGroup:
    line_number: int | None
    atoms: list[str]


def locate_line_number(atom: str, location_regex: str) -> int | None:
    match = re.search(location_regex, atom)
    if match is None:
        return None
    return int(match.group(1))


def group_atoms(atoms: list[str], location_regex: str) -> list[AtomGroup]:
    groups: list[AtomGroup] = []
    index_by_line: dict[int, int] = {}
    for atom in atoms:
        text = atom.strip()
        if not text:
            continue
        line = locate_line_number(text, location_regex)
        if line is None:
            groups.append(AtomGroup(line_number=None, atoms=[text]))
            continue
        if line in index_by_line:
            groups[index_by_line[line]].atoms.append(text)
        else:
            index_by_line[line] = len(groups)
            groups.append(AtomGroup(line_number=line, atoms=[text]))
    return groups


def atom_spans(atoms: list[str]) -> list[tuple[int, int]]:
    spans = []
    pos = 0
    for i, atom in enumerate(atoms):
        start = pos
        end = pos + len(atom)
        spans.append((start, end))
        pos = end + (1 if i < len(atoms) - 1 else 0)  # account for '\n'
    return spans


def covered_atom_indices(spans: list[tuple[int, int]], start: int, end: int) -> list[int]:
    return [i for i, (lo, hi) in enumerate(spans) if lo < end and hi > start]


def carve_group(
    atoms: list[str], patterns: list[tuple[object, str]]
) -> tuple[list[tuple[object, str]], list[str]]:
    remaining = list(atoms)
    hits: list[tuple[object, str]] = []
    while remaining:
        text = "\n".join(remaining)
        spans = atom_spans(remaining)
        claimed: list[int] | None = None
        for error_obj, pattern_str in patterns:
            match = re.search(pattern_str, text, re.MULTILINE)
            if match is None:
                continue
            if match.start() == match.end():
                continue  # skip zero-width
            indices = covered_atom_indices(spans, match.start(), match.end())
            if not indices:
                continue  # skip zero-atom covers
            claimed = indices
            matched_text = "\n".join(remaining[i] for i in indices)
            hits.append((error_obj, matched_text))
            break
        if claimed is None:
            break
        drop = set(claimed)
        remaining = [a for i, a in enumerate(remaining) if i not in drop]
    return hits, remaining
