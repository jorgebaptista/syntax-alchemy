# Booleans & Logical Operations

Boolean values, comparisons, and truthiness-based logic.

## Boolean Literals

- `true`
- `false`

Printing booleans shows `True` or `False`.

## Truthiness

Conditions and boolean operators use Python-style truthiness:

- `None`, `false`, `0`, `""`, and `[]` are false
- everything else is true

## Comparison Operators

Comparisons return booleans and work for integers, booleans, strings, lists, and
`None`. When comparing different types, the order is:

```
None < bool < int < string < list
```

List comparisons are lexicographic (element by element).

## Logical Operators

- `and` / `or` short-circuit and return one of their operands
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

**Short-circuit evaluation:**
```
print not true and 1 / 0 == 0
```
Output: `False` (doesn't evaluate `1/0` because left side is false)

See [test_booleans.exp](../../tests/test_booleans.exp) and
[test_logical.exp](../../tests/test_logical.exp) for more examples.
