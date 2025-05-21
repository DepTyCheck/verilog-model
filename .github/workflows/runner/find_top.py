import re

def find_top(file_content: str) -> str:
    """
    Find the top module name in the given file content.
    
    Args:
        file_content (str): The content of the file to search for the top module
        
    Returns:
        str: The name of the top module, or None if no top module is found
    """
    matches = re.findall(r'module \K[A-z]+', file_content, re.MULTILINE)
    if matches:
        return matches[-1]
    return None
