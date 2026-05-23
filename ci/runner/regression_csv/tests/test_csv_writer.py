import unittest

from regression_csv.csv_writer import format_csv


class TestFormatCsv(unittest.TestCase):
    def test_empty_rows_produces_header_only(self):
        self.assertEqual(format_csv([]), "example_id,type,is_reproduced\n")

    def test_single_true_row(self):
        rows = [("foo", "minified", True)]
        self.assertEqual(
            format_csv(rows),
            "example_id,type,is_reproduced\nfoo,minified,true\n",
        )

    def test_single_false_row(self):
        rows = [("foo", "full", False)]
        self.assertEqual(
            format_csv(rows),
            "example_id,type,is_reproduced\nfoo,full,false\n",
        )

    def test_rows_emitted_in_input_order(self):
        rows = [("b", "minified", True), ("a", "minified", False)]
        self.assertEqual(
            format_csv(rows),
            "example_id,type,is_reproduced\nb,minified,true\na,minified,false\n",
        )

    def test_example_id_with_special_chars_is_quoted(self):
        rows = [("foo,bar", "full", True)]
        self.assertEqual(
            format_csv(rows),
            'example_id,type,is_reproduced\n"foo,bar",full,true\n',
        )


if __name__ == "__main__":
    unittest.main()
