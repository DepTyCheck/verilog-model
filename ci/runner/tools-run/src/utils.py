from typing import List


def print_pretty(content: List[str]) -> None:
    sep = "\n" + "=" * 80
    text_to_print = sep + "\n" + "\n".join(content) + sep
    print(text_to_print)
