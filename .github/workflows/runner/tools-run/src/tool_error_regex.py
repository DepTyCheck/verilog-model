class ToolErrorRegex:
    def __init__(self, raw_str_regex: str):
        self.regex = raw_str_regex.rstrip("\n")
