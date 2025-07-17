import re
from ignored_errors_list import IgnoredErrorsList

def handle_errors(
    output: str,
    error_regex: str,
    ignored_errors: IgnoredErrorsList,
) -> bool:
    """
    Handle errors in the output by checking against ignored error patterns.
    
    Args:
        output (str): The command output to check
        error_regex (str): Regex pattern to find errors in output
        ignored_errors (IgnoredErrorsList): List of ignored error patterns
        file_content (str): Content of the file being tested
        
    Returns:
        bool: True if all errors are ignored, False otherwise
    """
    # Find all matches of the error regex in the output
    matches = re.finditer(error_regex, output, re.MULTILINE)
    
    if not matches:
        print("No errors matched.")
        return False

    all_errors_ignored = True

    # Check each match against ignored errors
    for match in matches:
        error_text = match.group(0)
        print(f"Matched error: {error_text}")
        if not ignored_errors.match(error_text):
            print(f"\033[91mFound unexpected error: {error_text}\033[0m\n")
            all_errors_ignored = False
            
    return all_errors_ignored
