#!/bin/bash

GEN_PATH="$1"
COMPILE_CMD="$2"
COMPILE_ERROR_REGEX="$3"
ERRORS_FILE="$4"
SIM_CMD="$5"
SIM_ERROR_REGEX="$6"

handle_errors() {
  local OUTPUT="$1"
  local ERROR_REGEX="$2"
  local ERRORS_FILE="$3"

  # A single command output may contain multiple errors. Process each one
  .github/workflows/filter_errors.sh "$OUTPUT" "$ERROR_REGEX" "$ERRORS_FILE" false
  FILTER_ERRORS_EXIT_CODE=$?

  # If the command returned a non-zero code but no error is found, analyze the whole output
  if [ $FILTER_ERRORS_EXIT_CODE -eq 0 ]; then
    echo "No specific errors found. Running broader error matching."
    .github/workflows/filter_errors.sh "$OUTPUT" "[\w\W]+" "$ERRORS_FILE" true
    FILTER_ERRORS_EXIT_CODE=$?
  fi

  if [ $FILTER_ERRORS_EXIT_CODE -ne 0 ]; then
    echo "Exit."
    exit 1
  fi
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
  echo "Compiling $FILE"

  execute_command COMPILATION_OUTPUT COMPILATION_EXIT_CODE "$COMPILE_CMD" "$FILE"

  if [ $COMPILATION_EXIT_CODE -ne 0 ]; then
    handle_errors "$COMPILATION_OUTPUT" "$COMPILE_ERROR_REGEX" "$ERRORS_FILE"
  else
    if [ -z "$SIM_CMD" ]; then continue; fi
    echo "Simulating $FILE"

    execute_command SIM_OUTPUT SIM_EXIT_CODE "$SIM_CMD" "$FILE"

    if [ $SIM_EXIT_CODE -ne 0 ]; then
      handle_errors "$SIM_OUTPUT" "$SIM_ERROR_REGEX" "$ERRORS_FILE"
    fi
  fi

  echo -e "\n-------------------------------------------------------------------------------------------------------------------------\n"
done
