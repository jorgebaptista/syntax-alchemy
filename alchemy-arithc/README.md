# alchemy-arithc

Compiler from Arith mini-language to x86-64 assembly.

**→ See [ROADMAP.md](../docs/ROADMAP.md) for development plan**

## What is Arith?

A simple imperative language with:
- [Arithmetic](docs/features/arithmetic.md) - integers, +, -, *, /
- [Variables](docs/features/variables.md) - global and local
- [Booleans](docs/features/booleans.md) - comparisons and logical ops
- [Control flow](docs/features/control-flow.md) - if/else, while loops
- `print` - output values

**Example program:**
```
set x = 10
if x > 5 then
  print x * (x + 1) / 2
else
  print 0
```

Compiles to x86-64 assembly (AT&T syntax).

## Build & Run

**Build:**
```bash
export PATH="../_opam/bin:$PATH"
dune build
```

**Compile a program:**
```bash
./src/arithc.bc <file.exp>
```

**Run (WSL/Linux):**
```bash
wsl gcc -g -no-pie <file.s> -o <file.out>
wsl ./<file.out>
```

**Test all:**
```bash
./tests/run_tests.sh
```

## How It Works

**Pipeline:** `.exp` → Lexer (ocamllex) → Parser (Menhir) → Compiler → `.s`

**Compilation strategy:**
- Stack-based evaluation
- Global vars: `.data` segment
- Local vars: stack frame (`%rbp`-relative)
- 16-byte alignment for printf calls

**Files:**
- `src/ast.ml` - Abstract syntax tree
- `src/lexer.mll` - Token definitions
- `src/parser.mly` - Grammar rules
- `src/compile.ml` - Code generation
- `src/x86_64.ml` - Assembly helpers

## References

- [x86-64 reference](docs/x86-64-reference.md)
- [Lab assignment](docs/lab-assignment.md) (Portuguese)
- [TD2 Mini-Python](../labs/td2-mini-python/) (interpreter reference)
