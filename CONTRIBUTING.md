# Contributing to Syntax Alchemy

Welcome to the lab! This guide covers the practical workflow for contributing.

**→ For development guidelines, project structure, and philosophy, see [docs/AGENTS.md](./docs/AGENTS.md)**

---

## Getting Started

### Prerequisites

- OCaml 5.4+ with opam
- Dune build system
- Git

### Initial Setup

```bash
git clone https://github.com/jorgebaptista/syntax-alchemy.git
cd syntax-alchemy

# For existing alchemies, install dependencies
cd alchemy-arithc
opam install . --deps-only --with-test
```

---

## Creating a New Alchemy

```bash
./scripts/new-alchemy.sh alchemy-name "Brief description"
```

**Then:**

1. Update `docs/LABS.md` with project entry
2. Implement in `src/`
3. Add tests in `tests/`
4. Update the project README

---

## Development Workflow

```bash
# 1. Create a branch
git checkout -b feat/your-feature-name

# 2. Make changes, test frequently
dune test

# 3. Format code
dune build @fmt

# 4. Run all tests
../scripts/test-all.sh

# 5. Commit clearly
git commit -m "feat(alchemy-arithc): Add register allocation"
```

---

## Pull Requests

1. Ensure all tests pass
2. Update documentation if needed
3. Keep commits atomic, conventional commits preferred
4. CI must pass before merge

**→ See [docs/AGENTS.md](./docs/AGENTS.md) for code quality expectations and guidelines.**

---

## Additional Resources

- **[docs/AGENTS.md](./docs/AGENTS.md)** — Development guidelines and structure
- **[docs/LABS.md](./docs/LABS.md)** — Project index and status

---

## License

By contributing, you agree that your contributions will be licensed under the [MIT License](./LICENSE).
