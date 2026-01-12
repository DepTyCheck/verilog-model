class UnexpectedError:
    """
    Represents an unexpected error that occurred during tool execution.

    Attributes:
        tool_output_error_text (str): The error message from the tool's output.
        test_file_path (str): The path to the test file that caused the error.
    """

    def __init__(self, tool_output_error_text: str, test_file_path: str) -> None:
        self.tool_output_error_text = tool_output_error_text
        self.test_file_path = test_file_path
