# ci/runner/common/tests/test_nvc_segv_regex.py
import unittest

from common.error_types import MatchingMode
from common.ignored_errors_list import IgnoredErrorsList

# Parentheses around the libc path must be escaped; otherwise they form a
# capturing group and the literal '(' / ')' in the crash dump are not matched.
NVC_SEGV_EMIT_STORE_REGEX = r"""\*\*\* Caught signal 11 \(SEGV_MAPERR\) \[address=.*, ip=.*] \*\*\*

\[.*] ..\/src\/util.c:724 signal_handler.lto_priv.0
\[.*] \(\/usr\/lib\/x86_64-linux-gnu\/libc.so.6\)
\[.*] ..\/src\/vcode.c:3854 emit_store
\[.*] ..\/src\/lower.c:11824 lower_ports
\[.*] ..\/src\/lower.c:12823 lower_instance
\[.*] ..\/src\/elab.c:1452 elab_lower.lto_priv.0
\[.*] ..\/src\/elab.c:2162 elab_architecture.lto_priv.0
\[.*] ..\/src\/elab.c:2781 elab
\[.*] ..\/src\/elab.c:715 elaborate
\[.*] ..\/src\/nvc.c:2561 process_command
\[.*] ..\/src\/nvc.c:2736 main"""

NVC_SEGV_EMIT_STORE_OUTPUT = """*** Caught signal 11 (SEGV_MAPERR) [address=0x558c00c64318, ip=0x5583fe5ced8b] ***

[0x5583fe5268a5] ../src/util.c:724 signal_handler.lto_priv.0
[0x7effdaa4532f] (/usr/lib/x86_64-linux-gnu/libc.so.6)
[0x5583fe5ced8b] ../src/vcode.c:3854 emit_store
[0x5583fe5ca1b1] ../src/lower.c:11824 lower_ports
[0x5583fe5ca1b1] ../src/lower.c:12823 lower_instance
[0x5583fe57cb22] ../src/elab.c:1452 elab_lower.lto_priv.0
[0x5583fe57d820] ../src/elab.c:2162 elab_architecture.lto_priv.0
[0x5583fe51f96c] ../src/elab.c:2781 elab
[0x5583fe51f96c] ../src/elab.c:715 elaborate
[0x5583fe520c38] ../src/nvc.c:2561 process_command
[0x5583fe518f24] ../src/nvc.c:2736 main

nvc 1.22-devel (1.21.0.r90.g15b9d0ac2) (Using LLVM 18.1.3) [x86_64-pc-linux-gnu]

Please report this bug at https://github.com/nickg/nvc/issues
"""


class TestNvcSegvRegex(unittest.TestCase):
    def test_segv_emit_store_stack_matches_whole_output(self):
        """nvc SEGV dumps are classified via WHOLE mode (tool regex is 'error: .*')."""
        ignored = IgnoredErrorsList.from_patterns(
            [NVC_SEGV_EMIT_STORE_REGEX], MatchingMode.WHOLE
        )
        found = ignored.match(NVC_SEGV_EMIT_STORE_OUTPUT, MatchingMode.WHOLE)
        self.assertIsNotNone(found)
        self.assertIn("Caught signal 11 (SEGV_MAPERR)", found.matched_text)
        self.assertIn("emit_store", found.matched_text)


if __name__ == "__main__":
    unittest.main()
