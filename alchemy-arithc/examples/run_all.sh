#!/bin/bash
# Script to run all examples and show their output

cd "$(dirname "$0")"
eval $(opam env) 2>/dev/null

echo "╔══════════════════════════════════════════════════════════════╗"
echo "║          Arith Language - Example Programs                  ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo

run_example() {
    local file=$1
    local name=$2
    
    echo "─────────────────────────────────────────────────────────────"
    echo "$name"
    echo "─────────────────────────────────────────────────────────────"
    
    if ../_build/default/src/arithc.bc "$file" 2>/dev/null; then
        local sfile="${file%.exp}.s"
        local outfile="${file%.exp}.out"
        if gcc -g -no-pie "$sfile" -o "$outfile" 2>/dev/null; then
            echo "Output:"
            "./$outfile"
            echo
        else
            echo "Assembly failed"
            echo
        fi
    else
        echo "Compilation failed"
        echo
    fi
}

# Algorithm examples
echo "ALGORITHMS"
run_example "factorial.exp" "Factorial (5! = 120)"
run_example "fibonacci.exp" "Fibonacci Sequence (first 10)"
run_example "gcd.exp" "Greatest Common Divisor"
run_example "power.exp" "Fast Exponentiation"
run_example "prime_checker.exp" "Prime Number Checker"

# List operations
echo "LIST OPERATIONS"
run_example "list_sum.exp" "Sum of List Elements"
run_example "list_max.exp" "Find Maximum in List"
run_example "bubble_sort.exp" "Bubble Sort Algorithm"
run_example "list_filter.exp" "Filter Even Numbers"

# String operations
echo "STRING OPERATIONS"
run_example "string_concat.exp" "String Concatenation"
run_example "string_comparison.exp" "String Comparisons"

# Language features
echo "LANGUAGE FEATURES"
run_example "let_expressions.exp" "Let Expressions & Polymorphism"
run_example "range_demo.exp" "List Range Generation"
run_example "arithmetic_demo.exp" "Arithmetic Operations"

# Advanced
echo "ADVANCED ALGORITHMS"
run_example "collatz.exp" "Collatz Conjecture"
run_example "tower_of_hanoi.exp" "Tower of Hanoi"
run_example "ackermann.exp" "Ackermann Function"

echo "╔══════════════════════════════════════════════════════════════╗"
echo "║                    All examples complete!                    ║"
echo "╚══════════════════════════════════════════════════════════════╝"
