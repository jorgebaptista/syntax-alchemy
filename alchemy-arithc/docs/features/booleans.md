# Booleans & Logical Operations

Boolean values, comparisons, and boolean logic.

## Boolean Literals

- `true`
- `false`

Printing booleans shows `True` or `False`.

## Conditions

`if`, `while`, and boolean operators require a boolean condition. There is no
implicit truthiness or automatic conversion from other types to booleans.

## Comparison Operators

Comparisons return booleans. Equality (`==`, `!=`) requires both operands to
have the same type. Ordering (`<`, `<=`, `>`, `>=`) is defined for integers,
strings, and lists whose element type is orderable (recursively).

List comparisons are lexicographic (element by element).

## Logical Operators

- `and` / `or` short-circuit and return a boolean
- `not` returns a boolean (`True` or `False`)

## Examples

**Comparisons:**
```
print 1 < 2
print 5 == 5
print 3 != 4
```
Output: `True`, `True`, `True`

**Logical operations:**
```
print true and false
print true or false
print not true
```
Output: `False`, `True`, `False`

See [test_booleans.exp](../../tests/test_booleans.exp) and
[test_logical.exp](../../tests/test_logical.exp) for more examples.
