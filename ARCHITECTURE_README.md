# fortfront Architecture - Reality Check

## What fortfront ACTUALLY Is

A **simple** Fortran frontend library that provides:
- Basic lexical analysis (tokenization)
- Simple parsing to AST
- Direct code generation

## What fortfront IS NOT

- ❌ NOT a compiler (no optimization, no machine code)
- ❌ NOT a type inference system (no Hindley-Milner)
- ❌ NOT an advanced analyzer (no complex static analysis)
- ❌ NOT a CST implementation (no concrete syntax trees)
- ❌ NOT an arena-based system (no custom memory management)

## Realistic Architecture

```
Input.f90 → Lexer → Tokens → Parser → AST → Codegen → Output.f90
```

That's the entire pipeline. Nothing more complex.

## Where to Look for Documentation

### USEFUL Documentation
- **DESIGN.md** - Realistic architecture (95 lines)
- **README.md** - How to build and use fortfront
- **docs/ERROR_HANDLING_GUIDE.md** - Error management patterns
- **docs/STATIC_LIBRARY_GUIDE.md** - Library integration

### IGNORE These Directories
- **DOCS/** - Contains unimplementable architecture fantasies
- **docs/** files mentioning CST/arena - Beyond team capability

## Development Reality

**Team Can Implement:**
- Fix build system issues
- Parse basic Fortran constructs
- Generate simple Fortran output
- Refactor oversized files
- Write basic tests

**Team Cannot Implement:**
- Advanced type systems
- Memory management systems
- Compiler optimizations
- Complex static analysis
- IDE integrations

## Getting Started

1. Read **DESIGN.md** for achievable goals
2. Check **BACKLOG.md** SPRINT_BACKLOG section
3. Focus on issues marked as HIGH PRIORITY
4. Ignore anything mentioning CST/arena/type-inference

---

*This document reflects honest team capability assessment after removing architectural complexity that caused development paralysis.*