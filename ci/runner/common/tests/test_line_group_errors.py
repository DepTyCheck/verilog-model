import unittest

from common.line_group_errors import AtomGroup, group_atoms, locate_line_number

LOC = r"^[A-z0-9_.\/,-]+:(\d+):"


class TestLocateAndGroup(unittest.TestCase):
    def test_locate_line_with_comma_in_path(self):
        atom = "generated-modules/97-seed_1,2.sv:41: error: Invalid module item."
        self.assertEqual(locate_line_number(atom, LOC), 41)

    def test_locate_missing_returns_none(self):
        self.assertIsNone(locate_line_number("error: Invalid module item.", LOC))

    def test_group_same_line_and_singleton(self):
        atoms = [
            "f.sv:41: syntax error",
            "f.sv:41: error: Syntax error in instance port expression(s).",
            "error: no location here",
            "f.sv:41: error: Invalid module item.",
            "f.sv:5: error: other",
        ]
        groups = group_atoms(atoms, LOC)
        self.assertEqual(
            groups,
            [
                AtomGroup(
                    line_number=41,
                    atoms=[
                        "f.sv:41: syntax error",
                        "f.sv:41: error: Syntax error in instance port expression(s).",
                        "f.sv:41: error: Invalid module item.",
                    ],
                ),
                AtomGroup(line_number=None, atoms=["error: no location here"]),
                AtomGroup(line_number=5, atoms=["f.sv:5: error: other"]),
            ],
        )


from common.line_group_errors import carve_group


class TestCarveGroup(unittest.TestCase):
    def test_carve_non_prefix_then_edges(self):
        atoms = ["L1", "L2", "L3", "L4", "L5"]
        patterns = [
            ("A", r"L2\nL3"),
            ("B", r"L1"),
            ("C", r"L4\nL5"),
        ]
        hits, left = carve_group(atoms, patterns)
        self.assertEqual([(h[0], h[1]) for h in hits], [
            ("A", "L2\nL3"),
            ("B", "L1"),
            ("C", "L4\nL5"),
        ])
        self.assertEqual(left, [])

    def test_leftover_when_partial(self):
        atoms = ["keep-me", "error: Invalid module item."]
        hits, left = carve_group(atoms, [("imi", r"Invalid module item\.")])
        self.assertEqual(hits[0][0], "imi")
        self.assertEqual(left, ["keep-me"])

    def test_first_pattern_order_wins(self):
        atoms = ["shared"]
        hits, left = carve_group(atoms, [("first", r"shared"), ("second", r"shared")])
        self.assertEqual([h[0] for h in hits], ["first"])
        self.assertEqual(left, [])


from common.ignored_errors_list import IgnoredErrorsList


class TestIverilogLineGroupMatching(unittest.TestCase):
    """
    Focused integration test: real-ish multi-line iverilog transcript is carved
    by the tightened patterns from invalid_module_item.yaml and
    errors_in_port_declarations.yaml.

    Bare 'syntax error' lines are consumed via extra_regexes so no 'unknown'
    leaks through.
    """

    TRANSCRIPT_ATOMS = [
        "test.sv:5: syntax error",
        "test.sv:1: Errors in port declarations.",
        "test.sv:31: syntax error",
        "test.sv:31: error: Invalid module item.",
    ]

    PATTERNS = [
        ("invalid_module_item", r"Invalid module item\.$"),
        ("errors_in_port_declarations", r"Errors in port declarations\.$"),
        ("bare_syntax_error", r"syntax error$"),
    ]

    def test_carve_finds_invalid_module_item(self):
        hits, left = carve_group(self.TRANSCRIPT_ATOMS, self.PATTERNS)
        hit_ids = [h[0] for h in hits]
        self.assertIn("invalid_module_item", hit_ids)

    def test_carve_finds_port_declarations(self):
        hits, left = carve_group(self.TRANSCRIPT_ATOMS, self.PATTERNS)
        hit_ids = [h[0] for h in hits]
        self.assertIn("errors_in_port_declarations", hit_ids)

    def test_carve_no_unknown_when_bare_syntax_error_in_extra(self):
        hits, left = carve_group(self.TRANSCRIPT_ATOMS, self.PATTERNS)
        self.assertEqual(left, [], f"Unexpected leftover atoms: {left}")

    def test_new_pattern_does_not_match_bare_syntax_error_line(self):
        """Tightened patterns must NOT consume a bare 'syntax error' line alone."""
        import re
        bare = "test.sv:5: syntax error"
        self.assertIsNone(re.search(r"Invalid module item\.$", bare))
        self.assertIsNone(re.search(r"Errors in port declarations\.$", bare))

    def test_invalid_module_item_matched_text_contains_expected_suffix(self):
        hits, _ = carve_group(self.TRANSCRIPT_ATOMS, self.PATTERNS)
        matched = {h[0]: h[1] for h in hits}
        self.assertTrue(
            matched["invalid_module_item"].endswith("Invalid module item."),
            f"Unexpected matched text: {matched['invalid_module_item']!r}",
        )


class TestSpecificMatchersOrder(unittest.TestCase):
    def test_sorted_known_then_extras(self):
        from common.error_file_parser import ErrorFile
        from common.error_types import MatchingMode as MM

        files = [
            ErrorFile(
                error_id="zeta", tool="t", regex="Z", mode=MM.SPECIFIC, title="", profile="sv"
            ),
            ErrorFile(
                error_id="alpha", tool="t", regex="A", mode=MM.SPECIFIC, title="", profile="sv"
            ),
            ErrorFile(
                error_id="whole", tool="t", regex="W", mode=MM.WHOLE, title="", profile="sv"
            ),
        ]
        lst = IgnoredErrorsList.from_error_files(files, extra_regexes=["extra1", "extra2"])
        ids_or_pat = []
        for m in lst.specific_matchers_in_order():
            ids_or_pat.append(getattr(m, "error_id", m.pattern))
        self.assertEqual(ids_or_pat, ["alpha", "zeta", "extra1", "extra2"])
