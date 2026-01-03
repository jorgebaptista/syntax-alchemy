# TD6 - Mini-Turtle (Logo Compiler)

**Status:** 🔮 Planned

## Overview

A complete lexer and parser for Mini-Turtle, a Logo-like language for turtle graphics.

## Topics Covered

1. **Lexer** - Tokens, comments, keywords
2. **Arithmetic expressions** - With correct precedence
3. **Atomic instructions** - `forward`, `penup`, `turnleft`, etc.
4. **Control structures** - `if`, `repeat`, blocks
5. **Functions** - Definition and calls

## Language Features

```logo
def square(size)
  repeat 4 {
    forward size
    turnleft 90
  }

color red
square(100)
```

## Reference

See [docs/labs/TD6.md](../../docs/labs/TD6.md) for the full lab instructions.
