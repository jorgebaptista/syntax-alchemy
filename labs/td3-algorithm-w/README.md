# TD3 - Algorithm W (Type Inference)

**Status:** 🔮 Planned

## Overview

Implementation of Algorithm W for Hindley-Milner type inference with destructive unification.

## Topics Covered

1. **Type representation** - `Tint`, `Tarrow`, `Tproduct`, `Tvar`
2. **Normalization** - `head` and `canon` functions
3. **Unification** - `occur` check and `unify`
4. **Free variables** - `fvars` computation
5. **Type environments** - Schemas and generalization
6. **Type inference** - The W algorithm

## How It Applies to Arith

This lab is **optional** for the main project. If you want to add type checking to Arith:

- Use `unify` to infer types of expressions
- Detect type errors at compile time (e.g., adding bool to int)
- Add type annotations to function parameters

## Reference

See [docs/labs/TD3.md](../../docs/labs/TD3.md) for the full lab instructions.
