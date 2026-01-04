# 🗺️ Arith Compiler Roadmap

Extending [TD1](labs/TD1.md) (base Arith) with features from [TD2](labs/TD%202.md) (Mini-Python interpreter).

## Phases

| Phase | Feature | Compile from TD2 | Status |
|-------|---------|------------------|--------|
| 1 | Constants, arithmetic, print, variables | — (TD1 base) | ✅ |
| 2 | Booleans, comparisons, if/else, while, for | `Sif`, `Sfor`, comparisons | ✅ |
| 3 | Functions (def/call/return, recursion) | `Ecall`, `Sreturn` | ✅ |
| 4 | Arrays (literals, indexing, len) | `Elist`, `Eget`, `Sset` | ✅ |
| 5 | Strings, None, modulo, ranges, expr statements | `Str`, `NoneLit`, `Mod`, `ListRange`, `Expr` | ✅ |

**TD3-6** are theory/practice exercises (optional for Arith). See [labs/README.md](../labs/README.md).

## Phase 4 - Arrays

- [x] Lexer: `[ ]`, `len`
- [x] Parser: list literals, indexing, len, element assignment
- [x] Compiler: heap-allocated lists, indexing and length
- [x] Tests: `test_lists`
- Syntax: `[e1, e2]`, `e[i]`, `len(e)`, `set a[i] = e`

## Phase 5 - Strings & None

- [x] Lexer: strings, `None`, `%`, `list`/`range`
- [x] Parser: string/None literals, `list(range(n))`, expression statements
- [x] Typechecker: string/none types, addable/comparable constraints
- [x] Compiler: string/list concat, comparisons, modulo, range, print support
- [x] Tests: `test_strings`, `test_none`, `test_modulo`, `test_range`, `test_list_ops`
- Syntax: `"s"`, `None`, `a + b`, `list(range(n))`, expression statements

## Commands

```bash
cd alchemy-arithc && dune build
./src/arithc.bc tests/test.exp
wsl gcc -g -no-pie tests/test.s -o tests/test.out && wsl ./tests/test.out
```
