import unittest

from src.run_command import find_top_entity_vhdl, find_top_module_sv


class TestRootEntityRegex(unittest.TestCase):
    def test_sv_top(self):
        with open("tests/data/generated-modules.sv", encoding="utf-8") as f:
            top = find_top_module_sv(f.read())
            self.assertEqual(top, "eqx")

    def test_vhdl_top(self):
        with open("tests/data/simle-design.vhdl", encoding="utf-8") as f:
            top = find_top_entity_vhdl(f.read())
            self.assertEqual(top, "lmao")
