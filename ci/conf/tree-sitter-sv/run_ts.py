import sys

import tree_sitter_systemverilog as tssv
from tree_sitter import Language, Parser


def main():
    if len(sys.argv) != 2:
        print("Usage: run_ts.py <file-to-parse>")
        sys.exit(1)

    file_path = sys.argv[1]

    parser = Parser(Language(tssv.language()))

    try:
        with open(file_path, "rb") as f:
            source_code = f.read()

        tree = parser.parse(source_code)

        if tree.root_node is not None:
            print(f"Successfully parsed {file_path}")
            sys.exit(0)
        else:
            print(f"Failed to parse {file_path}")
            sys.exit(1)
    except Exception as e:
        print(f"Error: {str(e)}")
        sys.exit(1)


if __name__ == "__main__":
    main()
