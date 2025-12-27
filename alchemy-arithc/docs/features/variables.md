# Variables

Two kinds of variables: global and local.

## Global Variables

Declared with `set` and stored in `.data` segment.

**Syntax:**
```
set <name> = <expr>
```

**Example:**
```
set x = 10
set y = x * x
print y + x
```
Output: `110`

**Compilation:** Allocated in `.data` segment as `.quad` values.

## Local Variables

Declared with `let...in` and stored on stack frame.

**Syntax:**
```
let <name> = <expr> in <expr>
```

**Example:**
```
print (let x = 3 in x * x)
```
Output: `9`

**Nested locals:**
```
print (let x = 3 in 
        (let y = x + x in x * y) + 
        (let z = x + 3 in z / z))
```
Output: `19`

**Compilation:** Allocated on stack relative to `%rbp` with negative offsets.

## Scope Rules

- Global variables are visible everywhere after declaration
- Local variables shadow outer variables with the same name
- Local variables only exist within their `let...in` expression

See [test_step3.exp](../../tests/test_step3.exp) (globals) and [test_step4.exp](../../tests/test_step4.exp) (locals) for more examples.
