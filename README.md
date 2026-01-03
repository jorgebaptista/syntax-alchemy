# ⚗️ Syntax Alchemy

**Transmuting syntax into meaning.**  
A growing collection of experimental interpreters, compilers, and language design studies — where abstract syntax is melted, purified, and reshaped into executable form.

Each project within *Syntax Alchemy* explores a new transformation:

- From high-level languages to machine code
- From expressions to evaluation
- From grammar to semantics

Welcome to the lab — where logic meets imagination.

<img width="1024" height="1024" alt="banner" src="https://github.com/user-attachments/assets/abb95299-9080-4714-a50a-edaa71c898c5" />

---

## 🧪 The Alchemies

Each experiment lives in its own `alchemy-*` directory, complete with source code, tests, and documentation.

| Project | Status | Description |
|---------|--------|-------------|
| [alchemy-arithc](./alchemy-arithc) | ⚗️ In Progress | Arith language compiler to x86-64 assembly |

**→ See [docs/ROADMAP.md](./docs/ROADMAP.md) for the development plan.**

---

## 📖 Labs

Independent learning exercises from the compiler course:

| Lab | Topic | Status |
|-----|-------|--------|
| [td2-mini-python](./labs/td2-mini-python) | Mini-Python interpreter | ✅ Complete |
| [td3-algorithm-w](./labs/td3-algorithm-w) | Type inference | 🔮 Planned |
| [td4-automata](./labs/td4-automata) | DFA from regex | 🔮 Planned |
| [td5-ll1-parser](./labs/td5-ll1-parser) | LL(1) parsing | 🔮 Planned |
| [td6-mini-turtle](./labs/td6-mini-turtle) | Turtle graphics | 🔮 Planned |

**→ See [labs/README.md](./labs/README.md) for how labs connect to the main project.**

---

## 🚀 Quick Start

```bash
# Create a new alchemy
./scripts/new-alchemy.sh alchemy-x86 "x86 compiler"

# Build and test
cd alchemy-x86
dune build && dune test
```

**→** See **[CONTRIBUTING.md](./CONTRIBUTING.md)** for detailed setup and workflow.

---

## 📚 Documentation

- **[AGENTS.md](./docs/AGENTS.md)** — Development guidelines, structure, and philosophy
- **[LABS.md](./docs/LABS.md)** — Project tracker and status
- **[CONTRIBUTING.md](./CONTRIBUTING.md)** — How to contribute

---

## 📜 License

MIT License - See [LICENSE](./LICENSE) for details.

---

*"From abstract syntax to executable form — the great work continues."*
