"""Print the original project directory from an upload-pack-state archive."""

import sys
import tarfile

if __name__ == "__main__":
    # upload-pack-state archives the project's build directory first.
    with tarfile.open(sys.argv[1]) as archive:
        build = archive.next()
    build_path = "/" + build.name.strip("/") if build is not None else ""
    suffix = "/" + sys.argv[2].strip("/")
    if build is None or not build.isdir() or not build_path.endswith(suffix):
        sys.exit("Expected the archived build directory as the first entry")
    print(build_path[: -len(suffix)])
