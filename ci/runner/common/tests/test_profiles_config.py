"""Tests for common/profiles_config.py."""

import tempfile
import unittest

from common.profiles_config import get_file_extension, load_profiles_config


def _write(body: str) -> str:
    with tempfile.NamedTemporaryFile("w", suffix=".yaml", delete=False) as f:
        f.write(body)
        return f.name


class TestProfilesConfig(unittest.TestCase):

    def test_load_roundtrips_nested(self):
        path = _write("sv:\n  file_extension: .sv\n  translate_hook: ci/scripts/sv.py\n")
        cfg = load_profiles_config(path)
        self.assertEqual(cfg["sv"]["file_extension"], ".sv")
        self.assertEqual(cfg["sv"]["translate_hook"], "ci/scripts/sv.py")

    def test_get_file_extension_known(self):
        self.assertEqual(get_file_extension("sv", {"sv": {"file_extension": ".sv"}}), ".sv")

    def test_get_file_extension_unknown_raises(self):
        with self.assertRaises(ValueError):
            get_file_extension("nope", {"sv": {"file_extension": ".sv"}})


if __name__ == "__main__":
    unittest.main()
