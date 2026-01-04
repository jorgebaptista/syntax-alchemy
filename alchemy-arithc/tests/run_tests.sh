#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
ARITH_WRAPPER=${1:?Path to arith wrapper is required}
ARITHC_PATH="${2:-}"

if [[ -n "$ARITHC_PATH" && "$ARITHC_PATH" != /* ]]; then
  ARITHC_PATH="$(cd -- "$(dirname -- "$ARITHC_PATH")" && pwd)/$(basename -- "$ARITHC_PATH")"
fi

run_case() {
  local name="$1"
  local exp_file="$SCRIPT_DIR/${name}.exp"
  local asm_file="$SCRIPT_DIR/${name}.s"
  local bin_file="$SCRIPT_DIR/${name}.out"
  local expected_file="$SCRIPT_DIR/${name}.expected"

  if [[ -n "$ARITHC_PATH" ]]; then
    ARITHC="$ARITHC_PATH" bash "$ARITH_WRAPPER" compile "$exp_file"
  else
    bash "$ARITH_WRAPPER" compile "$exp_file"
  fi
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

  if [[ -n "$ARITHC_PATH" ]]; then
    if output=$(ARITHC="$ARITHC_PATH" bash "$ARITH_WRAPPER" compile "$exp_file" 2>&1); then
      echo "Test ${name} expected failure but succeeded" >&2
      exit 1
    fi
  elif output=$(bash "$ARITH_WRAPPER" compile "$exp_file" 2>&1); then
    echo "Test ${name} expected failure but succeeded" >&2
    exit 1
  fi

  if ! diff -u "$expected_file" <(printf "%s\n" "$output"); then
    echo "Test ${name} failed" >&2
    exit 1
  fi
}

for case in test_step1 test_step2 test_step3 test_step4 test test_booleans test_conditionals test_logical test_loops test_functions test_recursion test_lists test_for test_strings test_none test_modulo test_range test_list_ops test_let_poly; do
  run_case "$case"
done

for case in test_type_error_add test_type_error_if test_type_error_fun_arity test_type_error_fun_arg test_return_outside test_type_error_list_assign test_type_error_string_add test_type_error_range; do
  run_error_case "$case"
done
