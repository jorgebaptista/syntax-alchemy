# alchemy-arithc

Simple compiler from Arith mini-language to x86-64 assembly.

## What it does

Takes programs like this:
```
set x = 10
print x * (x + 1) / 2
```

And compiles them to x86-64 assembly.

**Features:** arithmetic (`+`, `-`, `*`, `/`), global variables (`set`), local variables (`let`), printing (`print`)

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
set x = expr       # global variable
print expr         # print result
let x = e1 in e2   # local variable
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
