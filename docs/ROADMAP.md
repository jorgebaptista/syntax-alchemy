# 🗺️ Arith Compiler Roadmap

Extending [TD1](labs/TD1.md) (base Arith) with features from [TD2](labs/TD%202.md) (Mini-Python interpreter).

## Phases

| Phase | Feature | Compile from TD2 | Status |
|-------|---------|------------------|--------|
| 1 | Constants, arithmetic, print, variables | — (TD1 base) | ✅ |
| 2 | Booleans, comparisons, if/else, while | `Sif`, `Sfor`, comparisons | ✅ |
| 3 | Functions (def/call/return, recursion) | `Ecall`, `Sreturn` | 📋 |
| 4 | Arrays (literals, indexing, len) | `Elist`, `Eget`, `Sset` | 📋 |

**TD3-6** are theory/practice exercises (optional for Arith). See [labs/README.md](../labs/README.md).

## Current: Phase 3 - Functions

- [ ] Lexer: `DEF`, `RETURN`
- [ ] Parser: function definitions, calls
- [ ] Compiler: x86-64 calling convention, stack frames
- [ ] Tests: `test_functions`, `test_recursion`

## Commands

```bash
cd alchemy-arithc && dune build
./src/arithc.bc tests/test.exp
wsl gcc -g -no-pie tests/test.s -o tests/test.out && wsl ./tests/test.out
```
