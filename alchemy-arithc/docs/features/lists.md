# Lists

List literals, indexing, length, and element assignment.

## Literals

**Syntax:**
```
[e1, e2, ...]
```

**Example:**
```
set a = [1, 2, 3]
```

## Types

Lists can hold mixed types. You can replace elements with values of a different
type.

## Concatenation

Use `+` to concatenate two lists (creates a new list).

**Example:**
```
set a = [1, 2]
set b = [3]
set c = a + b
print c
```
Output: `[1, 2, 3]`

## Indexing

Access an element by index (0-based).

**Syntax:**
```
e[i]
```

**Example:**
```
print a[0]
```
Output: `1`

## Length

Get the number of elements.

**Syntax:**
```
len(e)
```

**Example:**
```
print len(a)
```
Output: `3`

## Ranges

Build a list of integers from `0` to `n-1` using `list(range(n))`.

**Syntax:**
```
list(range(n))
```

**Example:**
```
set r = list(range(4))
print r[3]
```
Output: `3`

## Element Assignment

Update a list element in place.

**Syntax:**
```
set a[i] = e
```

**Example:**
```
set a[1] = 10
print a[1]
```
Output: `10`

## Runtime Errors

These operations raise runtime errors on invalid inputs:

- `len` on a non-list
- indexing with a non-list or non-integer index
- out-of-range indexes
- assignment with a non-list or non-integer index

See [test_lists.exp](../../tests/test_lists.exp) for more examples.

**Related:** list iteration with `for` loops is documented in
[control-flow](control-flow.md).
