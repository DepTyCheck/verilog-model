import sys
import tree_sitter


def main():
    if len(sys.argv) != 3:
        print("Usage: run_ts.py <tree-sitter-sv-lib-path> <file-to-parse>")
        sys.exit(1)

    ts_lib_path = sys.argv[1]
    file_path = sys.argv[2]

    SV = tree_sitter.Language(ts_lib_path, "systemverilog")
    parser = tree_sitter.Parser()
    parser.set_language(SV)

    try:
        with open(file_path, "r", encoding="utf-8") as f:
            source_code = f.read()

        tree = parser.parse(bytes(source_code, "utf-8"))

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
