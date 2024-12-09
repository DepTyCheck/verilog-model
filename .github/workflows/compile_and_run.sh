#!/bin/bash

GEN_PATH="$1"
COMPILE_CMD="$2"
COMPILE_ERROR_REGEX="$3"
ERRORS_FILE="$4"
SIM_CMD="$5"
SIM_ERROR_REGEX="$6"

for FILE in "$GEN_PATH"/*.sv; do
  echo "Compiling $FILE"

  ACTUAL_COMPILATION_CMD="${COMPILE_CMD//\{file\}/$FILE}"
  echo "EXECUTE: $ACTUAL_COMPILATION_CMD"
  COMPILATION_OUTPUT="$(eval $ACTUAL_COMPILATION_CMD 2>&1)"
  COMPILATION_EXIT_CODE=$?

  echo "Exit code: $COMPILATION_EXIT_CODE. Output:"
  echo "$COMPILATION_OUTPUT"

  if [ $COMPILATION_EXIT_CODE -ne 0 ]; then
    .github/workflows/filter_errors.sh "$COMPILATION_OUTPUT" "$COMPILE_ERROR_REGEX" "$ERRORS_FILE"
    FILTER_ERRORS_EXIT_CODE=$?
    if [ $FILTER_ERRORS_EXIT_CODE -ne 0 ]; then
      echo "Exit."
      exit 1
    fi
  else
    if [ -z "$SIM_CMD" ]; then continue; fi
    echo "Simulating $FILE"

    ACTUAL_SIMULATION_CMD="${SIM_CMD//\{file\}/$FILE}"

    SIM_OUTPUT="$($ACTUAL_SIMULATION_CMD 2>&1)"
    SIM_EXIT_CODE=$?

    echo "Exit code: $SIM_EXIT_CODE. Output:"
    echo "$SIM_OUTPUT"

    if [ $SIM_EXIT_CODE -ne 0 ]; then
      .github/workflows/filter_errors.sh "$SIM_OUTPUT" "$SIM_ERROR_REGEX" "$ERRORS_FILE"
      FILTER_ERRORS_EXIT_CODE=$?
      if [ $FILTER_ERRORS_EXIT_CODE -ne 0 ]; then
        echo "Exit."
        exit 1
      fi
    fi
  fi

  echo "---------------------------------------------"
done
