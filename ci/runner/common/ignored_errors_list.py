from dataclasses import dataclass

from common.error_file_parser import parse_error_files
from common.error_types import MatchingMode
from cwm import Needle


@dataclass
class IgnoredErrorsList:
    needles: list[Needle]
    ignored_regexes: list[str]

    @classmethod
    def from_directory(cls, dir_path, tool, regex_list=None):
        return cls.from_error_files(parse_error_files(dir_path, tool=tool), regex_list)

    @classmethod
    def from_error_files(cls, error_files, extra_regexes=None):
        needles = [Needle(e.error_id, e.regex, "whole" if e.mode == MatchingMode.WHOLE else "usual") for e in error_files]
        return cls(needles, [r.rstrip("\n") for r in extra_regexes or []])

    @classmethod
    def from_patterns(cls, patterns, mode=MatchingMode.SPECIFIC):
        matching_mode = "whole" if mode == MatchingMode.WHOLE else "usual"
        return cls([Needle(f"e{i}", p, matching_mode) for i, p in enumerate(patterns)], [])
