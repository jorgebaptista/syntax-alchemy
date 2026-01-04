# Arithmetic Operations

Integer constants and basic arithmetic operators.

## Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `3 + 4` → `7` |
| `-` | Subtraction | `10 - 3` → `7` |
| `*` | Multiplication | `5 * 6` → `30` |
| `/` or `//` | Integer division | `15 / 4` → `3` |
| `%` | Modulo | `10 % 3` → `1` |
| `( )` | Grouping | `(2 + 3) * 4` → `20` |

## Examples

**Simple arithmetic:**
```
print 1 + 2 * 3
```
Output: `7`

**Order of operations:**
```
print (3 * 3 + 4 * 4) / 5
```
Output: `5`

**Negation:**
```
print 10 - 3 - 4
```
Output: `3`

**Complex expression:**
```
print 4 + 7 / 2
```
Output: `7`

**Modulo:**
```
print 10 % 3
```
Output: `1`

## Compilation

All operations use stack-based evaluation:
1. Push operands onto stack
2. Pop operands, perform operation
3. Push result back

See [test_step2.exp](../../tests/test_step2.exp) for more examples.
