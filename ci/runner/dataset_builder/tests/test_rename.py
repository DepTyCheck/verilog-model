import unittest

from dataset_builder.src.rename import rename_file


class TestRenameFile(unittest.TestCase):
    def test_sv_file(self):
        result = rename_file("86-seed_3969165351885200449,3128299129089410139.sv", "2026_04_24")
        self.assertEqual(result, "2026_04_24-seed_3969165351885200449_3128299129089410139.sv")

    def test_vhdl_file(self):
        result = rename_file("191-seed_6199300995674822752,5224943229413370507.vhdl", "2026_04_24")
        self.assertEqual(result, "2026_04_24-seed_6199300995674822752_5224943229413370507.vhdl")

    def test_large_index(self):
        result = rename_file("9999-seed_111,222.sv", "2026_04_24")
        self.assertEqual(result, "2026_04_24-seed_111_222.sv")

    def test_invalid_pattern_raises(self):
        with self.assertRaises(ValueError):
            rename_file("bad_name.sv", "2026_04_24")

    def test_no_index_raises(self):
        with self.assertRaises(ValueError):
            rename_file("seed_111,222.sv", "2026_04_24")

    def test_comma_in_seed_replaced_with_underscore(self):
        result = rename_file("1-seed_10,20.sv", "2026_04_24")
        self.assertNotIn(",", result)
        self.assertIn("_10_20", result)


if __name__ == "__main__":
    unittest.main()
