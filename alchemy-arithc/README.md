# alchemy-arithc

Simple compiler from Arith mini-language to x86-64 assembly.

## What it does

Takes programs like this:
```
set x = 10
if x > 5 then
  print x * (x + 1) / 2
else
  print 0

set i = 0
while i < 5 do
  print i
  set i = i + 1
done
```

And compiles them to x86-64 assembly.

**Features:**
- Arithmetic (`+`, `-`, `*`, `/`)
- Booleans (`true`, `false`)
- Comparisons (`==`, `!=`, `<`, `<=`, `>`, `>=`)
- Logical operators (`and`, `or`, `not`) with short-circuit evaluation
- Global variables (`set`)
- Local variables (`let...in`)
- Conditionals (`if...then...else`)
- Loops (`while...do...done`)
- Print (`print`)

## Structure

```
src/compile.ml         The compiler
src/parser.mly         Parser (Menhir)
src/lexer.mll          Lexer (ocamllex)
tests/*.exp            Test programs
examples/generated/    Example assembly output
```

## Quick Start

```bash
# Setup (first time only)
export PATH="../_opam/bin:$PATH"

# Build
dune build

# Compile a program
./src/arithc.exe tests/test.exp

# On Linux: run it
gcc -g -no-pie test.s -o test.out && ./test.out
```

Expected output: `60, 50, 0, 10, 55, 60, 20, 43`

## How it works

**Pipeline:** `.exp` → Lexer → Parser → Compiler → `.s` assembly

**Strategy:**
- Stack-based: everything gets pushed/popped
- Global vars in `.data` segment
- Local vars on stack frame (`%rbp`-relative)
- 16-byte stack alignment for printf

## Language syntax

```
# Variables
set x = expr                    # global variable
let x = e1 in e2                # local variable (in expressions)

# Output
print expr                      # print result

# Control flow
if cond then stmt else stmt     # conditional
if cond then e1 else e2         # conditional expression
while cond do stmts done        # loop

# Operators
+ - * /                         # arithmetic
== != < <= > >=                 # comparison
and or not                      # logical (short-circuit)
true false                      # boolean literals
```

Operators: `+`, `-`, `*`, `/`, parentheses

Example:
```
set x = 10
print (let y = x + 1 in x * y)   # prints 110
```

## Docs

- [Lab assignment](./docs/lab-assignment.md) (Portuguese)
- [x86-64 reference](./docs/x86-64-reference.md)
