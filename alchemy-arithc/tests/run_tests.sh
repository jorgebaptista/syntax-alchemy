#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
ARITHC=${1:?Path to arithc executable is required}

run_case() {
  local name="$1"
  local exp_file="$SCRIPT_DIR/${name}.exp"
  local asm_file="$SCRIPT_DIR/${name}.s"
  local bin_file="$SCRIPT_DIR/${name}.out"
  local expected_file="$SCRIPT_DIR/${name}.expected"

  "$ARITHC" "$exp_file"
  gcc -g -no-pie "$asm_file" -o "$bin_file"

  if ! diff -u "$expected_file" <("$bin_file"); then
    echo "Test ${name} failed" >&2
    exit 1
  fi
}

run_error_case() {
  local name="$1"
  local exp_file="$SCRIPT_DIR/${name}.exp"
  local expected_file="$SCRIPT_DIR/${name}.expected"
  local output

  if output=$("$ARITHC" "$exp_file" 2>&1); then
    echo "Test ${name} expected failure but succeeded" >&2
    exit 1
  fi

  if ! diff -u "$expected_file" <(printf "%s\n" "$output"); then
    echo "Test ${name} failed" >&2
    exit 1
  fi
}

for case in test_step1 test_step2 test_step3 test_step4 test test_booleans test_conditionals test_logical test_loops; do
  run_case "$case"
done

for case in test_type_error_add test_type_error_if; do
  run_error_case "$case"
done
