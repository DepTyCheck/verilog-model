import re
from ignored_errors_list import IgnoredErrorsList

def handle_errors(
    output: str,
    error_regex: str,
    ignored_errors: IgnoredErrorsList,
) -> list[str]:
    """
    Handle errors in the output by checking against ignored error patterns.
    Args:
        output (str): The command output to check
        error_regex (str): Regex pattern to find errors in output
        ignored_errors (IgnoredErrorsList): List of ignored error patterns
    Returns:
        list[str]: List of error texts that are not ignored
    """
    matches = list(re.finditer(error_regex, output, re.MULTILINE))
    if not matches:
        print("No errors matched.")
        return []
    found_errors = []
    for match in matches:
        error_text = match.group(0)
        print(f"Matched error: {error_text}")
        if not ignored_errors.match(error_text):
            print(f"\033[91mFound unexpected error: {error_text}\033[0m\n")
            found_errors.append(error_text)
    return found_errors
