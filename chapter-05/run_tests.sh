#!/bin/bash

PHASE="${1}"
shift

for INPUT in $*; do
  echo -----------------------------
  echo "${INPUT}"
  echo -----------------------------
  if ! grep "${INPUT}" errors.txt; then
    cat -n "${INPUT}"
    echo
    echo -----------------------------
    dist/build/Tiger/Tiger "${PHASE}" "${INPUT}"
    if [ "$?" -ne 0 ]; then
      break
    fi
  fi
done
