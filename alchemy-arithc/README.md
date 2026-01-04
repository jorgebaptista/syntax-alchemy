# alchemy-arithc

Compiler from Arith mini-language to x86-64 assembly.

**→ See [ROADMAP.md](../docs/ROADMAP.md) for development plan**

## What is Arith?

A simple imperative language with:

- [Arithmetic](docs/features/arithmetic.md) - integers, +, -, *, /, %
- [Variables](docs/features/variables.md) - global and local
- [Type checking](docs/features/type-checking.md) - static inference, polymorphic let
- [Booleans](docs/features/booleans.md) - comparisons and logical ops
- [Strings](docs/features/strings.md) - literals, concatenation, comparisons
- [Control flow](docs/features/control-flow.md) - if/else, while, for loops
- [Lists](docs/features/lists.md) - literals, indexing, len, concat, range
- [Functions](docs/features/functions.md) - def/return, calls, recursion
- [None](docs/features/none.md) - literal and default returns
- `print` - output values

**Example program:**

```text
set x = 10
if x > 5 then
  print x * (x + 1) / 2
else
  print 0
```

**Functions (define before top-level statements):**

```text
def add(x, y) (
  return x + y
)
print add(2, 3)
```
Output: `5`

Compiles to x86-64 assembly (AT&T syntax).

## Build & Run

> **Requires:** WSL/Linux with OCaml 5.4.0, opam, dune, menhir, and gcc.

**Setup (first time):**

```bash
# In WSL Ubuntu
opam init -y && opam switch create default 5.4.0 -y
eval $(opam env)
opam install dune menhir -y
```

**Build the compiler:**

```bash
eval $(opam env)
cd alchemy-arithc && dune build
```

**Quick start with wrapper script:**

```bash
./arith run tests/test_step1.exp      # compile and run
./arith build examples/demo.exp       # compile to executable
./arith compile tests/test.exp        # compile to assembly only
```

**Manual compilation (what the script does):**

```bash
./_build/default/src/arithc.bc <file.exp>   # Arith → assembly (.s)
gcc -g -no-pie <file.s> -o <file.out>       # assembly → binary
./<file.out>                                 # run
```

**Test all:**

```bash
dune test
```

## How It Works

**Pipeline:** `.exp` (Arith) → **arithc** → `.s` (x86-64 asm) → **gcc** → binary

The compiler (`arithc`) translates Arith to assembly. GCC assembles and links with libc (for `printf`).

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
