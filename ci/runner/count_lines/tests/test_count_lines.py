import os
import tempfile
import unittest
from pathlib import Path

from count_lines.count_lines import GroupCount, LineCounts, _is_printer_file, _is_pure_code_line, count_file, count_lines, format_report

DATA_DIR = Path(os.path.dirname(os.path.abspath(__file__))) / "data" / "src"

# Expected values derived from test data files:
#   Runner.idr:   8 total, 4 pure  (printer)
#   Pretty.idr:   6 total, 3 pure  (printer)
#   Spec.idr:     7 total, 3 pure  (specification)
PRINTER_TOTAL = 14
PRINTER_PURE = 7
SPEC_TOTAL = 7
SPEC_PURE = 3


class TestIsPrinterFile(unittest.TestCase):
    def test_runner_is_printer(self):
        self.assertTrue(_is_printer_file(Path("src/Runner.idr")))

    def test_pretty_is_printer(self):
        self.assertTrue(_is_printer_file(Path("src/Test/Pretty.idr")))

    def test_printable_is_printer(self):
        self.assertTrue(_is_printer_file(Path("src/Test/PrintableDesigns.idr")))

    def test_vhdl_pretty_is_printer(self):
        self.assertTrue(_is_printer_file(Path("src/Test/VHDL/Pretty.idr")))

    def test_spec_is_not_printer(self):
        self.assertFalse(_is_printer_file(Path("src/Test/Spec.idr")))

    def test_design_is_not_printer(self):
        self.assertFalse(_is_printer_file(Path("src/Test/Common/Design.idr")))

    def test_case_insensitive_pretty(self):
        self.assertTrue(_is_printer_file(Path("src/PRETTY.idr")))


class TestIsPureCodeLine(unittest.TestCase):
    def test_empty_line_is_not_pure(self):
        self.assertFalse(_is_pure_code_line(""))

    def test_whitespace_only_is_not_pure(self):
        self.assertFalse(_is_pure_code_line("   \n"))

    def test_line_comment_is_not_pure(self):
        self.assertFalse(_is_pure_code_line("-- this is a comment"))

    def test_indented_line_comment_is_not_pure(self):
        self.assertFalse(_is_pure_code_line("  -- indented comment"))

    def test_doc_comment_is_not_pure(self):
        self.assertFalse(_is_pure_code_line("||| doc string"))

    def test_indented_doc_comment_is_not_pure(self):
        self.assertFalse(_is_pure_code_line("  ||| indented doc"))

    def test_code_line_is_pure(self):
        self.assertTrue(_is_pure_code_line("myFunc : Nat -> Nat"))

    def test_module_declaration_is_pure(self):
        self.assertTrue(_is_pure_code_line("module Foo.Bar"))

    def test_import_is_pure(self):
        self.assertTrue(_is_pure_code_line("import Foo"))

    def test_pipe_operator_not_confused_with_doc(self):
        # || is not |||
        self.assertTrue(_is_pure_code_line("  x || y"))


class TestCountFile(unittest.TestCase):
    def _make_file(self, content: str) -> Path:
        with tempfile.NamedTemporaryFile(mode="w", suffix=".idr", delete=False, encoding="utf-8") as f:
            f.write(content)
            return Path(f.name)

    def test_empty_file(self):
        p = self._make_file("")
        total, pure = count_file(p)
        self.assertEqual(total, 0)
        self.assertEqual(pure, 0)
        p.unlink()

    def test_only_comments_and_blanks(self):
        p = self._make_file("-- comment\n\n||| doc\n")
        total, pure = count_file(p)
        self.assertEqual(total, 3)
        self.assertEqual(pure, 0)
        p.unlink()

    def test_mixed_content(self):
        content = "module Foo\n\n-- comment\n||| doc\nfoo : Nat\nfoo = 1\n"
        p = self._make_file(content)
        total, pure = count_file(p)
        self.assertEqual(total, 6)
        self.assertEqual(pure, 3)
        p.unlink()

    def test_indented_comments_inside_namespace(self):
        # Simulates a namespace block where -- and ||| are indented
        content = "namespace MyNS\n  -- indented line comment\n  ||| indented doc comment\n  foo : Nat\n  foo = 0\n"
        p = self._make_file(content)
        total, pure = count_file(p)
        self.assertEqual(total, 5)
        self.assertEqual(pure, 3)  # namespace decl + foo : Nat + foo = 0
        p.unlink()

    def test_deeply_indented_comments(self):
        # Tabs and multiple spaces before -- / |||
        content = (
            "namespace Outer\n"
            "  namespace Inner\n"
            "\t\t-- tab-indented comment\n"
            "\t\t||| tab-indented doc\n"
            "\t\tbar : String\n"
            '\t\tbar = "x"\n'
        )
        p = self._make_file(content)
        total, pure = count_file(p)
        self.assertEqual(total, 6)
        self.assertEqual(pure, 4)  # 2 namespace decls + bar : String + bar = "x"
        p.unlink()

    def test_comment_not_confused_with_code_containing_double_dash(self):
        # A string literal containing "--" is still a code line (no leading --)
        content = 'label : String\nlabel = "a--b"\n-- real comment\n'
        p = self._make_file(content)
        total, pure = count_file(p)
        self.assertEqual(total, 3)
        self.assertEqual(pure, 2)
        p.unlink()


class TestCountLines(unittest.TestCase):
    def setUp(self):
        self.counts = count_lines(DATA_DIR)

    def test_printer_total(self):
        self.assertEqual(self.counts.printers.total, PRINTER_TOTAL)

    def test_printer_pure(self):
        self.assertEqual(self.counts.printers.pure, PRINTER_PURE)

    def test_spec_total(self):
        self.assertEqual(self.counts.specification.total, SPEC_TOTAL)

    def test_spec_pure(self):
        self.assertEqual(self.counts.specification.pure, SPEC_PURE)

    def test_grand_total_total(self):
        self.assertEqual(self.counts.total_total, PRINTER_TOTAL + SPEC_TOTAL)

    def test_grand_total_pure(self):
        self.assertEqual(self.counts.total_pure, PRINTER_PURE + SPEC_PURE)


class TestFormatReport(unittest.TestCase):
    def test_format_output(self):
        counts = LineCounts(
            printers=GroupCount(total=100, pure=60),
            specification=GroupCount(total=200, pure=150),
        )
        report = format_report(counts)
        lines = report.splitlines()
        self.assertEqual(lines[0], "Total lines (including comments and documentation):")
        self.assertIn("100 printers", lines[1])
        self.assertIn("200 specification", lines[1])
        self.assertIn("300 total", lines[1])
        self.assertEqual(lines[2], "")
        self.assertEqual(lines[3], "Source lines of code:")
        self.assertIn("60 printers", lines[4])
        self.assertIn("150 specification", lines[4])
        self.assertIn("210 total", lines[4])
