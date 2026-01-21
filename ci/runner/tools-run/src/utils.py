from typing import List

from src.logger import get_logger


def print_pretty(content: List[str]) -> None:
    sep = "\n" + "=" * 80
    text_to_print = sep + "\n".join(content) + sep
    get_logger().info(text_to_print)
