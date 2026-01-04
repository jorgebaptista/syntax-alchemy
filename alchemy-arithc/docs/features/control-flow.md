# Control Flow

Conditionals and loops for program control.

## If-Then-Else (Statement)

Execute statements based on condition.

**Syntax:**
```
if <condition> then
  <statements>
else
  <statements>
```

**Example:**
```
set x = 10
if x > 5 then
  print 1
else
  print 0
```
Output: `1`

**Compilation:** Uses conditional jumps (`jz`) to else/end labels.

## If-Then-Else (Expression)

Evaluate to different values based on condition.

**Syntax:**
```
if <condition> then <expr> else <expr>
```

**Example:**
```
print if 3 != 4 then 10 * 2 else 14
```
Output: `20`

## While Loops

Repeat statements while condition is true.

**Syntax:**
```
while <condition> do
  <statements>
done
```

**Example (count to 5):**
```
set i = 0
while i < 5 do
  print i
  set i = i + 1
done
```
Output: `0, 1, 2, 3, 4`

**Example (factorial):**
```
set n = 5
set result = 1
while n > 0 do
  set result = result * n
  set n = n - 1
done
print result
```
Output: `120`

**Example (sum 1-10):**
```
set i = 1
set sum = 0
while i <= 10 do
  set sum = sum + i
  set i = i + 1
done
print sum
```
Output: `55`

## For Loops

Iterate over a list (evaluated once).

**Syntax:**
```
for <name> in <list-expr> do
  <statements>
done
```

**Example:**
```
set sum = 0
for x in [1, 2, 3] do
  set sum = sum + x
done
print sum
```
Output: `6`

**Compilation:** stores the list pointer and length, then loops over indices.

## Expression Statements

Any expression can be used as a statement; its value is ignored. This is
useful for calling functions with side effects.

**Example:**
```
def set_first(a) (
  set a[0] = 42
)
set xs = [1, 2]
set_first(xs)
print xs[0]
```
Output: `42`

## Compilation

- **If:** Label at start of else block, label at end
- **While:** Label at start of loop, condition check with `jz` to end, `jmp` back to start

## Nested Control Flow

Can nest if/while arbitrarily:
```
set i = 0
while i < 3 do
  if i == 1 then
    print 100
  else
    print i
  set i = i + 1
done
```
Output: `0, 100, 2`

See [test_conditionals.exp](../../tests/test_conditionals.exp) and [test_loops.exp](../../tests/test_loops.exp) for more examples.
