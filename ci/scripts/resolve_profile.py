#!/usr/bin/env python3
"""Resolve a profile's options from profiles.yaml into KEY=VALUE lines for $GITHUB_ENV.

Prints `TRANSLATE_HOOK=<hook>` and `FILE_PATTERN=*<file_extension>` for the given
profile. Exits non-zero if the profile is absent or an option is missing.
"""

import argparse
import sys

import yaml


def main() -> None:
    ap = argparse.ArgumentParser(description="Resolve translate-hook and file-pattern for a profile.")
    ap.add_argument("--profiles", required=True, help="Path to profiles.yaml")
    ap.add_argument("--profile", required=True, help="Profile name to resolve")
    args = ap.parse_args()

    with open(args.profiles, encoding="utf-8") as f:
        profiles = yaml.safe_load(f)

    if not isinstance(profiles, dict) or args.profile not in profiles:
        print(f"Unknown profile {args.profile!r} in {args.profiles}", file=sys.stderr)
        sys.exit(1)

    opts = profiles[args.profile]
    try:
        hook = opts["translate_hook"]
        ext = opts["file_extension"]
    except (TypeError, KeyError) as exc:
        print(f"Profile {args.profile!r} missing required option: {exc}", file=sys.stderr)
        sys.exit(1)

    print(f"TRANSLATE_HOOK={hook}")
    print(f"FILE_PATTERN=*{ext}")


if __name__ == "__main__":
    main()
