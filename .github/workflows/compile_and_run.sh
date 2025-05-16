#!/bin/bash

GEN_PATH="$1"
COMPILE_CMD="$2"
COMPILE_ERROR_REGEX="$3"
ERRORS_FILE="$4"
SIM_CMD="$5"
SIM_ERROR_REGEX="$6"

FAILED_TESTS=0
FAILED_FILES=()

print_file() {
  local FILE="$1"
  echo ""
  echo "The entire content of $FILE :"
  cat "$FILE"
  echo ""
}

handle_errors() {
  local OUTPUT="$1"
  local ERROR_REGEX="$2"
  local ERRORS_FILE="$3"
  local FILE_CONTENT="$4"

  # A single command output may contain multiple errors. Process each one
  .github/workflows/filter_errors.sh "$OUTPUT" "$ERROR_REGEX" "$ERRORS_FILE" false
  FILTER_ERRORS_EXIT_CODE=$?

  # If the command returned a non-zero code but no error is found, analyze the whole output
  if [ $FILTER_ERRORS_EXIT_CODE -eq 0 ]; then
    echo "No specific errors found. Running broader error matching."
    .github/workflows/filter_errors.sh "$FILE_CONTENT\n$OUTPUT" "[\w\W]+" "$ERRORS_FILE" true
    FILTER_ERRORS_EXIT_CODE=$?
  fi

  if [ $FILTER_ERRORS_EXIT_CODE -ne 0 ]; then
    print_file "$FILE"
    FAILED_TESTS=$((FAILED_TESTS + 1))
    FAILED_FILES+=("$FILE")

    return 1
  fi

  return 0
}

execute_command() {
  local -n OUTPUT_VAR="$1"
  local -n EXIT_VAR="$2"
  local CMD_TEMPLATE="$3"
  local FILE="$4"

  local CMD="${CMD_TEMPLATE//\{file\}/$FILE}"
  echo "Execute: $CMD"

  OUTPUT_VAR="$(eval "$CMD" 2>&1)"
  EXIT_VAR=$?

  echo "Exit code: $EXIT_VAR. Output:"
  echo "$OUTPUT_VAR"
}

for FILE in "$GEN_PATH"/*.sv; do
  FILE_CONTENT="$(cat "$FILE")"

  echo "Compiling $FILE"

  execute_command COMPILATION_OUTPUT COMPILATION_EXIT_CODE "$COMPILE_CMD" "$FILE"

  if [ $COMPILATION_EXIT_CODE -ne 0 ]; then
    handle_errors "$COMPILATION_OUTPUT" "$COMPILE_ERROR_REGEX" "$ERRORS_FILE" "$FILE_CONTENT"
  else
    if [ -z "$SIM_CMD" ]; then continue; fi
    echo "Simulating $FILE"

    execute_command SIM_OUTPUT SIM_EXIT_CODE "$SIM_CMD" "$FILE"

    if [ $SIM_EXIT_CODE -ne 0 ]; then
      handle_errors "$SIM_OUTPUT" "$SIM_ERROR_REGEX" "$ERRORS_FILE" "$FILE_CONTENT"
    fi
  fi

  echo -e "\n-------------------------------------------------------------------------------------------------------------------------\n"
done

echo ""
echo "==========================================="
echo "  Total failed tests: $FAILED_TESTS"
if [ $FAILED_TESTS -ne 0 ]; then
  echo "  Failed files:"
  for f in "${FAILED_FILES[@]}"; do
    echo "  - $f"
  done
  echo "==========================================="
  exit 1
else
  echo "  All tests passed successfully."
  echo "==========================================="
  exit 0
fi
