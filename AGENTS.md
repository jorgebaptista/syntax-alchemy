# 🤖 AGENTS.md

**Development Guidelines for Syntax Alchemy**

## Current Focus: alchemy-arithc

Compiler from Arith mini-language to x86-64 assembly. Based on [TD1](labs/TD1.md), extended with features from [TD2](labs/TD%202.md).

**→ See [ROADMAP.md](ROADMAP.md) for phases and tasks.**

## Key Principles

1. **DRY** - Link to existing docs, don't duplicate
2. **Test First** - Add tests before implementing features
3. **Incremental** - Small changes, verify with tests
4. **Clarity** - Code teaches; comments explain "why"

## Project Structure

```
syntax-alchemy/
├── alchemy-arithc/        # Main project - x86-64 compiler
│   ├── src/               # ast.ml, lexer.mll, parser.mly, compile.ml
│   └── tests/             # test_*.exp + test_*.expected
├── labs/                  # Learning exercises (TD2-TD6)
│   └── td2/               # Reference implementation
    └── td3/
    └── .../
├── docs/
│   ├── ROADMAP.md         # Development plan
│   ├── labs/              # Lab instructions (TD1-TD6)
└── _opam/                 # OCaml toolchain
```

## How Labs Connect

| Lab | Purpose | Use for Arith |
|-----|---------|---------------|
| TD1 | Base compiler | ✅ Already implemented |
| TD2 | Interpreter with control flow, functions, lists | Compile same features to x86-64 |
| TD3 | Type inference | Optional: add type checking |
| TD4 | Lexer theory | Understand lexer.mll |
| TD5 | Parser theory | Understand parser.mly |
| TD6 | Another compiler (Turtle) | Practice |

## Environment

- **Build**: MSYS2 bash (`dune build`)
- **Run**: WSL Ubuntu (`gcc -g -no-pie *.s && ./a.out`)
- **OCaml**: 5.4.0 in `_opam/`

## Workflow

```bash
# Build
cd alchemy-arithc && dune build

# Test single
./src/arithc.bc tests/test.exp
wsl gcc -g -no-pie tests/test.s -o tests/test.out && wsl ./tests/test.out

# Test all
for t in tests/*.exp; do ./src/arithc.bc $t; done
wsl -d Ubuntu -e bash -c 'cd /mnt/d/.../alchemy-arithc && for t in tests/*.s; do gcc -g -no-pie $t -o ${t%.s}.out && ./${t%.s}.out; done'
```

## Code Style

- OCaml: Use pattern matching, avoid mutable state except where needed
- Assembly: AT&T syntax, stack-based evaluation, 16-byte alignment for printf
- Tests: One `.exp` (source) + `.expected` (output) per feature

---

*"From abstract syntax to executable form — the great work continues."*
