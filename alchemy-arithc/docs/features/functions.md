# Functions

Define functions, call them as expressions, and use recursion.

## Definition

Functions are declared before top-level statements.

**Syntax:**
```
def <name>(param1, param2, ...) (
  <statements>
)
```

**Example:**
```
def add(x, y) (
  return x + y
)
```

## Calls

Calls are expressions and can appear anywhere an expression is allowed.

**Example:**
```
print add(2, 3)
```
Output: `5`

## Return

Use `return <expr>` to return a value from a function.

If a function reaches the end without a return, it returns `0`.

## Scope

`set` inside a function creates or updates a local variable. Functions do not
capture top-level variables; pass them as parameters instead.

## Recursion

Functions can call themselves.

**Example:**
```
def fact(n) (
  if n <= 1 then return 1 else return n * fact(n - 1)
)
print fact(5)
```
Output: `120`

See [test_functions.exp](../../tests/test_functions.exp) and
[test_recursion.exp](../../tests/test_recursion.exp) for more examples.
