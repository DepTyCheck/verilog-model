#!/bin/bash

for file in "$GENERATION_DIR"/*.sv; do
  if [[ -f "$file" && "$file" == *,* ]]; then
    new_name="${file//,/_}"
    echo "Renaming: $file -> $new_name"
    mv "$file" "$new_name"
  fi
done
