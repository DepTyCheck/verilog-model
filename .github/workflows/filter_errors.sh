#!/bin/bash

INPUT="$1"
ERRORS_REGEX="$2"
REGEX_FILE="$3"

# Find all errors using the provided regex pattern
matches=$(echo "$INPUT" | grep -Po "(?s)$ERRORS_REGEX")

if [ -z "$matches" ]; then
  echo "No errors found."
  exit 0
fi

# Loop through each match
while IFS= read -r MATCH; do
  echo "Match: $MATCH"
  # Check for matches against regex patterns
  FOUND_MATCH=false
  while read -r PATTERN; do
    if [ -z "$PATTERN" ]; then continue; fi
    if echo "$MATCH" | grep -qE "$PATTERN"; then
      echo "Ignore. Pattern: $PATTERN"
      FOUND_MATCH=true
      break
    fi
  done < "$REGEX_FILE"

  if [ "$FOUND_MATCH" != true ] ; then
    echo "The unexpected error was found!"
    exit 1
  fi
done <<< "$matches"
