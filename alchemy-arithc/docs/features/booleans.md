# Booleans & Logical Operations

Boolean values, comparison operators, and logical operations with short-circuit evaluation.

## Boolean Literals

- `true` → compiles to `1`
- `false` → compiles to `0`

## Comparison Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `==` | Equal | `5 == 5` → `true` |
| `!=` | Not equal | `3 != 4` → `true` |
| `<` | Less than | `2 < 3` → `true` |
| `<=` | Less or equal | `3 <= 3` → `true` |
| `>` | Greater than | `5 > 3` → `true` |
| `>=` | Greater or equal | `4 >= 4` → `true` |

## Logical Operators

| Operator | Description | Short-circuit? |
|----------|-------------|----------------|
| `and` | Logical AND | ✅ Yes |
| `or` | Logical OR | ✅ Yes |
| `not` | Logical NOT | N/A |

## Examples

**Comparisons:**
```
print 1 < 2
print 5 == 5
print 3 != 4
```
Output: `1` (true), `1` (true), `1` (true)

**Logical operations:**
```
print true and false
print true or false
print not true
```
Output: `0` (false), `1` (true), `0` (false)

**Short-circuit evaluation:**
```
print not true and 1 / 0 == 0
```
Output: `0` (doesn't evaluate `1/0` because left side is false)

## Compilation

- **Comparisons:** Use `cmpq` + `setCC` instructions (sete, setne, setl, setle, setg, setge)
- **Logical AND/OR:** Use conditional jumps for short-circuit evaluation
- **NOT:** Use `xorq $1` to flip 0↔1

See [test_booleans.exp](../../tests/test_booleans.exp) and [test_logical.exp](../../tests/test_logical.exp) for more examples.
