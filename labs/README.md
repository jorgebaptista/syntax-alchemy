# 🧪 Labs

Independent learning exercises from the compiler course. Each lab teaches concepts that are then applied to extend the main Arith compiler.

## Structure

| Lab | Topic | Description | Status |
|-----|-------|-------------|--------|
| [td2-mini-python](./td2-mini-python/) | Interpreter | Mini-Python interpreter with functions, loops, lists | ✅ |
| [td3-algorithm-w](./td3-algorithm-w/) | Type Inference | Algorithm W for ML-style type inference | 🔮 |
| [td4-automata](./td4-automata/) | Lexer Theory | DFA construction from regular expressions | 🔮 |
| [td5-ll1-parser](./td5-ll1-parser/) | Parser Theory | LL(1) parsing with FIRST/FOLLOW sets | 🔮 |
| [td6-mini-turtle](./td6-mini-turtle/) | Compiler | Logo/Turtle graphics compiler | 🔮 |

## How Labs Connect to Arith

```
TD2 (Mini-Python)     → Learn interpretation of control flow, functions, lists
                        Apply as COMPILATION in Arith Phases 2-4
                        
TD3 (Algorithm W)     → Learn type inference and unification
                        Optionally add type checking to Arith
                        
TD4 (Automata)        → Understand how lexer.mll works internally
                        
TD5 (LL(1) Parser)    → Understand parsing theory behind parser.mly
                        
TD6 (Mini-Turtle)     → Practice building another complete compiler
```

---

*See [ROADMAP.md](../docs/ROADMAP.md) for the full development plan.*
