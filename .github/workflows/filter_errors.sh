#!/bin/bash

INPUT="$1"
ERRORS_REGEX="$2"
REGEX_FILE="$3"
CONCAT="$4"

# Find all errors using the provided regex pattern
if [ "$CONCAT" = true ]; then
  matches=$(echo "$INPUT" | grep -Po "(?s)$ERRORS_REGEX" | tr '\n' ' ')
else
  matches=$(echo "$INPUT" | grep -Po "(?s)$ERRORS_REGEX")
fi

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
    if echo "$MATCH" | grep -qP "$PATTERN"; then
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
