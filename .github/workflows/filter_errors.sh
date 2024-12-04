#!/bin/bash

OUTPUT_TEXT="$1"
REGEX_FILE="$2"

# Check for matches against regex patterns
while read -r PATTERN; do
  if echo "$OUTPUT_TEXT" | grep -qE "$PATTERN"; then
    echo "An expected error was found. Ignore. Pattern: $PATTERN"
    exit 0
  fi
done < "$REGEX_FILE"

# If no pattern matched
echo "The unexpected error was found."
exit 1
