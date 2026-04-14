"""Tests for common/make_command.py"""

import unittest

from common.make_command import find_top_entity_vhdl, find_top_module_sv, make_command


class TestFindTopEntityVhdl(unittest.TestCase):

    def test_lowercase(self):
        self.assertEqual(find_top_entity_vhdl("entity foo is\nend foo;"), "foo")

    def test_uppercase(self):
        self.assertEqual(find_top_entity_vhdl("ENTITY my_entity IS\nEND my_entity;"), "my_entity")

    def test_mixed_case(self):
        self.assertEqual(find_top_entity_vhdl("Entity Foo Is\nEnd Foo;"), "Foo")

    def test_returns_last_entity(self):
        content = "entity first is\nend first;\nentity last is\nend last;"
        self.assertEqual(find_top_entity_vhdl(content), "last")

    def test_underscore_in_name(self):
        self.assertEqual(find_top_entity_vhdl("entity my_top_entity is\nend;"), "my_top_entity")

    def test_no_entity_raises(self):
        with self.assertRaises(Exception):
            find_top_entity_vhdl("-- no entity here\n")


class TestFindTopModuleSv(unittest.TestCase):

    def test_simple(self):
        self.assertEqual(find_top_module_sv("module foo; endmodule"), "foo")

    def test_returns_last_module(self):
        content = "module first; endmodule\nmodule last; endmodule"
        self.assertEqual(find_top_module_sv(content), "last")

    def test_no_module_raises(self):
        with self.assertRaises(Exception):
            find_top_module_sv("// just a comment\n")


class TestMakeCommand(unittest.TestCase):

    def test_file_placeholder(self):
        self.assertEqual(make_command("run {file}", "/tmp/f.sv", ""), "run /tmp/f.sv")

    def test_top_module_placeholder(self):
        cmd = make_command("sim {top_module}", "/tmp/f.sv", "module my_mod; endmodule")
        self.assertEqual(cmd, "sim my_mod")

    def test_vhdl_top_entity_placeholder_uppercase(self):
        cmd = make_command("ghdl -e {vhdl_top_entity}", "/tmp/f.vhdl", "ENTITY top IS\nEND top;")
        self.assertEqual(cmd, "ghdl -e top")

    def test_no_placeholder(self):
        self.assertEqual(make_command("echo hello", "/tmp/f.sv", ""), "echo hello")
