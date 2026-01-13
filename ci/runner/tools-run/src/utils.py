from typing import List


def print_pretty(content: List[str]) -> None:
    print("\n" + "=" * 80)
    for line in content:
        print(line)
    print("=" * 80)
