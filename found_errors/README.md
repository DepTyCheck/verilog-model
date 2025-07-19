# Format

Each file documents a single found error, identified by a unique id. There are generated and minified examples which reproduce the error.

Keys:
- title: A short, descriptive title for the error.
- tool: The name of the tool where the error was encountered.
- short_desc: A concise summary of the error.
- issue_status: The current status of the issue (e.g., reported, already_known, wont_report, unsupported).
- issue_link: A URL to the related issue, if available.
- stage: The stage or context in which the error was found (e.g., parsing, synthesis, simulation).
- examples: A list of examples that repoduce the error, each with:
    - first_found: The date the error was first observed.
    - minified_example: A minimal code snippet that triggers the error.
    - minified_error: The error message produced by the minimal example.
    - full_error: The complete error message from the tool.
    - full_example: The full code example that triggers the error.
    - commit_hash: The commit hash of the tool version used
    - version: The version of the tool in which the error was found or tested.
