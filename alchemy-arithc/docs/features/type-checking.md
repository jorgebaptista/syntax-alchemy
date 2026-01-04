# Type Checking

Arith uses static type inference with unification (W-style for local `let`).
Type errors are reported at compile time.

## Types

- `int`, `bool`, `string`, `none`
- `list[T]` (homogeneous)
- function types for `def`/calls

## Polymorphic `let`

`let x = e1 in e2` generalizes type variables that are not in scope, so the
binding can be instantiated at different types inside `e2`.

Example:
```
print (let xs = [] in len(xs + [1]) + len(xs + ["a"]))
```
Output: `2`

## Monomorphic bindings

- `set` variables are monomorphic (a single type across the program).
- Functions are monomorphic (each `def` has one inferred type).

This keeps mutation predictable while still allowing polymorphic local `let`.
