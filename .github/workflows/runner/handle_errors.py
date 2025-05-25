import re
from ignored_errors_list import IgnoredErrorsList

def print_file(file_content: str, file_path: str) -> None:
    """Print the contents of a file."""
    print(f"\nThe entire content of {file_path}:")
    print(file_content)
    print("")

def handle_errors(
    output: str,
    error_regex: str,
    ignored_errors: IgnoredErrorsList,
    file_content: str,
    file_path: str,
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
    try:
        # Find all matches of the error regex in the output
        matches = re.finditer(error_regex, output, re.MULTILINE)
        
        if not matches:
            print("No errors matched.")
            return False

        # Check each match against ignored errors
        for match in matches:
            error_text = match.group(0)
            print(f"Matched error: {error_text}")
            if not ignored_errors.match(error_text):
                print(f"Found unignored error: {error_text}\n")
                print_file(file_content, file_path)
                return False
                
        return True
        
    except re.error as e:
        print(f"Error in error regex pattern: {e}")
        return False
