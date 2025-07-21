import re
from ignored_errors_list import FoundMatch, IgnoredErrorsList, MatchingMode, UnexpectedErrorText


def extract_and_classify_errors(
    output: str,
    error_regex: str,
    ignored_errors: IgnoredErrorsList,
) -> tuple[list[UnexpectedErrorText], list[FoundMatch]]:
    """
    Extract errors from the output and classify them as ignored or unexpected.
    Args:
        output (str): The command output to check
        error_regex (str): Regex pattern to find errors in output
        ignored_errors (IgnoredErrorsList): List of ignored error patterns
    Returns:
        list[str]: List of error texts that are not ignored
        list[FoundMatch]: Matched errors
    """
    matches = list(re.finditer(error_regex, output, re.MULTILINE))
    if not matches:
        print("Warning: No errors matched.\n")
        return [], []

    found_errors: list[UnexpectedErrorText] = []
    found_matches: list[FoundMatch] = []

    for match in matches:
        error_text = match.group(0)
        print(f"Matched error: {error_text}")
        found_match = ignored_errors.match(error_text, mode=MatchingMode.SPECIFIC)
        if found_match == None:
            print(f"\033[91mFound unexpected error: {error_text}\033[0m\n")
            found_errors.append(error_text)
        else:
            found_matches.append(found_match)

    return found_errors, found_matches


def match_whole_output(
    output: str,
    ignored_errors: IgnoredErrorsList,
) -> FoundMatch | None:
    """
    Check if the whole output matches any ignored error pattern.
    Args:
        output (str): The command output to check
        ignored_errors (IgnoredErrorsList): List of ignored error patterns
    Returns:
        str | None: The actual matched string from the output if found, otherwise None
    """
    found_match = ignored_errors.match(output, mode=MatchingMode.WHOLE)
    if found_match == None:
        print(f"\033[91mCould't match whole output\033[0m\n")
        return None
    else:
        return found_match
