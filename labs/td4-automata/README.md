# TD4 - DFA from Regular Expressions

**Status:** 🔮 Planned

## Overview

Direct construction of deterministic finite automata from regular expressions. This is the algorithm used by tools like `ocamllex`.

## Topics Covered

1. **Nullability** - `null : regexp -> bool`
2. **First/Last sets** - `first`/`last : regexp -> Cset.t`
3. **Follow sets** - `follow : ichar -> regexp -> Cset.t`
4. **DFA construction** - `make_dfa : regexp -> autom`

## How It Applies to Arith

Understanding this helps you understand:
- How `lexer.mll` is compiled into an efficient automaton
- Why certain regex patterns are faster than others
- How to debug lexer issues

## Reference

See [docs/labs/TD4.md](../../docs/labs/TD4.md) for the full lab instructions.
