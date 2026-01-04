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

See [test_lists.exp](../../tests/test_lists.exp) for more examples.

**Related:** list iteration with `for` loops is documented in
[control-flow](control-flow.md).
