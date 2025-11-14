# Documentation Directory

## Purpose

This directory contains comprehensive technical documentation for fortfront's architecture, design decisions, implementation strategies, and usage patterns. Documentation covers memory safety, type system design, parsing algorithms, monomorphization strategy, and library integration.

## Documentation Index

### Architecture and Design

| Document | Description |
|----------|-------------|
| PRATT_PIPELINE_ARCHITECTURE.md | Pratt parser implementation for expressions, operator precedence |
| SEMANTIC_PIPELINE_ARCHITECTURE.md | Semantic analysis pipeline, type inference architecture |
| MEMORY_SAFETY_ANALYSIS.md | Arena allocation, memory safety guarantees, stack analysis |
| TYPE_SAFETY_GUIDE.md | Type system design, type checking, type inference rules |
| AST_MIGRATION.md | AST architecture evolution, migration from legacy designs |

### Implementation Guides

| Document | Description |
|----------|-------------|
| MONOMORPHIZATION.md | Complete monomorphization strategy for lazy Fortran |
| MONOMORPHIZATION_IMPLEMENTATION.md | Implementation details for monomorphization |
| MONOMORPHIZATION_STATUS.md | Current status and migration progress |
| LIBRARY_USAGE.md | API usage examples for tool developers |
| ECOSYSTEM.md | Integration with fortrun and package managers |

### Reference

| Document | Description |
|----------|-------------|
| NODE_TYPE_IDENTIFICATION.md | AST node type patterns and identification |
| CHARACTER_TYPE_GUIDE.md | Character type handling in Fortran |
| MIXED_CONSTRUCTS_GUIDE.md | Handling `.lf` files with embedded standard Fortran |
| PARSE_DECLARATION_REFACTORING.md | Parser refactoring history for declarations |
| pratt_parser_design.md | Original Pratt parser design notes |

### Development

| Document | Description |
|----------|-------------|
| LOCAL_VERIFICATION_EVIDENCE.md | Local testing and verification procedures |
| perf/ | Performance analysis and benchmarking notes |

## Key Documentation

### Essential Reading
1. **LIBRARY_USAGE.md** - Start here for using fortfront as a library
2. **MONOMORPHIZATION.md** - Understand lazy Fortran type inference strategy
3. **PRATT_PIPELINE_ARCHITECTURE.md** - Parser implementation details
4. **SEMANTIC_PIPELINE_ARCHITECTURE.md** - Semantic analysis design

### For Library Users
- **LIBRARY_USAGE.md** - API examples, integration patterns
- **ECOSYSTEM.md** - Integration with build systems
- **TYPE_SAFETY_GUIDE.md** - Type system overview

### For Contributors
- **MEMORY_SAFETY_ANALYSIS.md** - Memory management patterns
- **PRATT_PIPELINE_ARCHITECTURE.md** - Parser implementation
- **SEMANTIC_PIPELINE_ARCHITECTURE.md** - Semantic analysis
- **NODE_TYPE_IDENTIFICATION.md** - AST node patterns

### For Maintainers
- **AST_MIGRATION.md** - Architecture evolution
- **PARSE_DECLARATION_REFACTORING.md** - Refactoring history
- **MONOMORPHIZATION_STATUS.md** - Feature status

## Documentation Standards

**Format**: Markdown (`.md`)

**Structure**:
1. Purpose (what problem does this solve)
2. Design decisions (why this approach)
3. Implementation details (how it works)
4. Examples (code samples)
5. References (related documentation)

**Clarity**:
- Write for developers unfamiliar with fortfront internals
- Use examples liberally
- Explain "why" not just "what"
- Link to related documentation

**Maintenance**:
- Keep documentation up-to-date with code
- Mark outdated sections clearly
- Include date of last update
- Reference specific code files/functions

## Dependencies

None - documentation is standalone text.
