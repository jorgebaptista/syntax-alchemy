# 🤖 AGENTS.md

**AI Collaboration Guide for Syntax Alchemy**

## Project Philosophy

Syntax Alchemy is an experimental laboratory for language design, interpreters, and compilers. Each "alchemy" is a self-contained transformation project exploring how syntax becomes executable meaning.

**Key Principles:**
- **Modularity**: Each alchemy-* project is independent
- **Experimentation**: Favor exploration over perfection
- **Clarity**: Code should teach; comments explain the "why"
- **Rigor**: All alchemies must have tests

## Repository Structure

```
syntax-alchemy/
├── alchemy-*/              # Individual projects (e.g., alchemy-x86, alchemy-python)
│   ├── README.md          # Project overview, build/run instructions
│   ├── src/               # Source code
│   ├── tests/             # Test suite
│   └── dune-project       # OCaml build config (if applicable)
├── .github/workflows/     # CI/CD pipelines
├── docs/                  # Documentation (this file, LABS.md)
├── scripts/               # Automation tools
└── README.md             # Main landing page
```

## Development Guidelines

### Creating a New Alchemy

1. **Use the template generator:**
   ```bash
   ./scripts/new-alchemy.sh alchemy-name "Description"
   ```

2. **Each alchemy must have:**
   - Clear README.md (purpose, build, run, test)
   - Organized src/ directory
   - Comprehensive tests/
   - Build configuration (dune for OCaml, or language-specific)

3. **Register in docs/LABS.md:**
   - Add entry with status, tech stack, brief description

### Code Quality Expectations

- **Functional First**: Prefer immutability and pure functions (especially in OCaml)
- **Type Safety**: Leverage strong typing; avoid `Obj.magic` unless absolutely necessary
- **Documentation**: Public functions/modules need docstrings
- **Testing**: Unit tests for core logic, integration tests for end-to-end behavior
- **Error Handling**: Fail fast with clear messages; use Result/Option types in OCaml

### Language-Specific Notes

**OCaml Projects:**
- Use Dune build system
- Follow standard library conventions (Base/Core if used)
- Format code with `ocamlformat`
- Use `.mli` interface files for modules

**Other Languages:**
- Document tooling in project README
- Ensure CI supports the language
- Follow idiomatic patterns for that language

## CI/CD Context

All alchemies are automatically:
- Built on push/PR
- Tested against their test suites
- Checked for formatting (where applicable)

**Workflow files:**
- `ocaml-ci.yml` - Builds/tests OCaml projects
- `multi-lang.yml` - Detects and handles other languages

**Adding CI for a new alchemy:**
- CI auto-detects projects in `alchemy-*/` directories
- Add language-specific steps to relevant workflow
- Ensure tests are runnable via standard commands

## Working with AI Agents

**When starting work on an alchemy:**
1. Read the project's README.md first
2. Check docs/LABS.md for status and context
3. Explore existing code before making changes
4. Run tests before and after modifications

**When creating new features:**
1. Update tests first (TDD preferred)
2. Implement incrementally
3. Keep commits atomic and descriptive
4. Update README if behavior changes

**When debugging:**
1. Check CI logs for build/test failures
2. Reproduce locally before fixing
3. Add regression tests for bugs

## Communication Style

- Be concise but thorough
- Explain "why" for non-obvious decisions
- Use code comments for complex algorithms
- Update docs when architecture changes

## Project Goals

This repo serves multiple purposes:
- **Learning Lab**: Deep dive into language implementation
- **Portfolio**: Showcase compiler/interpreter design skills
- **Experimentation**: Try new ideas without constraints
- **DevOps Practice**: Modern CI/CD and automation

Remember: Every alchemy is a transmutation experiment. Some succeed brilliantly, others teach through failure. Both are valuable.

---

*"From abstract syntax to executable form — the great work continues."*
