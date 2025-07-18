#!/bin/sh

# SOURCE: https://github.com/buzden/idris2-ncd-xz/blob/f4df0657e21182e55710eeacb6324301d1337a51/ncd-xz

if ! xz --version > /dev/null 2> /dev/null; then
  echo "Cannot find \`xz\` command" >&2
  exit 2
fi

fileExists() {
  if [ -z "$1" ]; then
    echo "Usage: $0 <file1> <file2>" >&2
    exit 3
  fi

  if [ ! -r "$1" ]; then
    echo "Argument \"$1\" must be a readable file" >&2
    exit 4
  fi
}

FILE_A="$1"
FILE_B="$2"

fileExists "$FILE_A"
fileExists "$FILE_B"

# Prints a count of bytes of a compressed files
compSize() {
  cat "$@" | xz --format=raw --compress --stdout --no-warn --quiet | wc -c
}

A="$(compSize "$FILE_A")"
B="$(compSize "$FILE_B")"
AB="$(compSize "$FILE_A" "$FILE_B")"
BA="$(compSize "$FILE_B" "$FILE_A")"
AA="$(compSize "$FILE_A" "$FILE_A")"
BB="$(compSize "$FILE_B" "$FILE_B")"

echo "
scale=10
define min(a, b) { if (a < b) return a else return b }
define max(a, b) { if (a > b) return a else return b }
(min($AB, $BA) - min($AA, $BB)) / max($A, $B)
" | bc
