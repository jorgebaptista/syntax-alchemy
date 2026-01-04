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
5. **Test Execution** - Always run relevant tests and report results (or explain why they could not run)
6. **Docs** - When adding new functionality, update or add a doc under `alchemy-arithc/docs/features/` and link it from `alchemy-arithc/README.md`
7. **Formatting** - Run `dune fmt` before finishing a session

## Project Structure

```
syntax-alchemy/
├── alchemy-arithc/        # Main project - x86-64 compiler
│   ├── src/               # ast.ml, lexer.mll, parser.mly, compile.ml
│   └── tests/             # test_*.exp + test_*.expected
├── labs/                  # Learning exercises (TD2-TD6)
│   └── td2/               # Reference implementation
│   └── td3/
│   └── .../
├── docs/
│   ├── ROADMAP.md         # Development plan
│   └── labs/              # Lab instructions (TD1-TD6)
└── scripts/               # Helper scripts
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

> **All development uses WSL Ubuntu or native Linux.**

- **OCaml**: 5.4.0 via opam
- **Build**: `dune build`
- **Format**: `ocamlformat` (0.28.1), `dune format-dune-file`
- **Compile/Run**: `gcc -g -no-pie *.s && ./a.out`

## Workflow

```bash
# Enter environment
eval $(opam env)
cd alchemy-arithc

# Build
dune build

# Compile and run single test
./_build/default/src/arithc.bc tests/test.exp
gcc -g -no-pie tests/test.s -o tests/test.out && ./tests/test.out

# Run all tests
dune test
```

## Code Style

- OCaml: Use pattern matching, avoid mutable state except where needed
- Assembly: AT&T syntax, stack-based evaluation, 16-byte alignment for printf
- Tests: One `.exp` (source) + `.expected` (output) per feature

---

*"From abstract syntax to executable form — the great work continues."*
