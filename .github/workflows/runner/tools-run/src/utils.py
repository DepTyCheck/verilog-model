from typing import List


def print_pretty(content: List[str]) -> None:
    print(
        "\n======================================================================================"
    )
    for line in content:
        print(line)
    print(
        "======================================================================================"
    )
