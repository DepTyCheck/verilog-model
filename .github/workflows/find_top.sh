#!/bin/bash

if [ $# -ne 1 ]; then
  echo "Usage: $0 path_to_sv_file"
  exit 1
fi

FILE="$1"

if [ ! -f "$FILE" ]; then
  echo "File not found: $FILE"
  exit 1
fi

LAST_MODULE=$(grep -oP 'module \K[A-z]+' "$FILE" | tail -n 1)

if [ -n "$LAST_MODULE" ]; then
  echo "$LAST_MODULE"
else
  echo "No module found in file."
  exit 1
fi
