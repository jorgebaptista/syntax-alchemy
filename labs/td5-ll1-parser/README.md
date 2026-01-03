# TD5 - LL(1) Parser

**Status:** 🔮 Planned

## Overview

Construction of an LL(1) parser using FIRST, FOLLOW, and NULL sets.

## Topics Covered

1. **Fixpoint computation** - Generic iteration until stable
2. **NULL set** - Non-terminals that can derive ε
3. **FIRST set** - First terminals in derivations
4. **FOLLOW set** - Terminals that can follow a non-terminal
5. **Expansion table** - LL(1) parsing table construction

## How It Applies to Arith

Understanding this helps you understand:

- How `parser.mly` works internally
- Why certain grammars cause conflicts
- How to design unambiguous grammars

## Reference

See [docs/labs/TD5.md](../../docs/labs/TD5.md) for the full lab instructions.
