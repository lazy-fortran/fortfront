# Source Code (`src/`)

## Purpose

This directory contains the complete implementation of the fortfront compiler frontend. It includes all subsystems from lexing to code generation, organized by functional responsibility. The source is designed as a library first, with the CLI application in a separate `app/` directory.

## Architecture Overview

For the complete architecture overview including pipeline stages, core subsystems, and design patterns, see [CLAUDE.md Architecture Overview](../CLAUDE.md#architecture-overview).

**Quick Pipeline Summary**:
```
Source Text → [Lexer] → [Parser] → [Semantic] → [Standardizers] → [Codegen] → Standard Fortran
```

## Subsystem Organization

### Core Pipeline (Compilation Phases)

**1. `lexer/`** - Tokenization
- First phase: Source text → Token stream
- Recognizes keywords, identifiers, literals, operators
- Tracks source locations for error reporting

**2. `parser/`** - Parsing
- Second phase: Tokens → AST
- Recursive descent + Pratt parser for expressions
- Organized subdirectories: core, declarations, expressions, statements, procedures, control_flow
- Creates AST in arena memory

**3. `semantic/`** - Semantic Analysis
- Third phase: AST → Typed AST
- Type inference for lazy Fortran
- Type validation for standard Fortran
- Subdirectories: analyzers, types

**4. `codegen/`** - Code Generation
- Final phase: Typed AST → Standard Fortran
- Emits declarations, statements, procedures
- Handles indentation and formatting

### Supporting Infrastructure

**`analysis/`** - Program Analysis
- Call graph construction
- Variable usage tracking
- Operates on typed AST (after semantic analysis)

**`standardizers/`** - AST Standardization
- Bridge between semantic analysis and codegen
- Inserts inferred declarations
- Adds intent attributes
- Handles monomorphization

**`frontend/`** - Pipeline Orchestration
- High-level API for transformation
- Coordinates all pipeline stages
- Handles mixed constructs (lazy + standard Fortran)

### Data Structures

**`ast/`** - Abstract Syntax Tree
- Central data structure for all compilation phases
- Subdirectories: nodes, traversal, arena, factory
- Arena-based allocation (no manual deallocation)
- Visitor pattern for traversal

**`cst/`** - Concrete Syntax Tree
- Optional lossless source representation
- Preserves formatting, comments, whitespace
- For tools that need exact source details

### Utilities and Support

**`memory/`** - Memory Management
- Arena allocators
- Compiler-wide allocation context
- Predictable performance, no fragmentation

**`common/`** - Shared Utilities
- Identifier interning (string to ID)
- UID generation
- Declaration attribute handling

**`utilities/`** - General Utilities
- String manipulation
- Debug tracing
- CLI environment handling
- Input validation

**`interfaces/`** - C API Bindings
- C-compatible interface
- Enables integration with non-Fortran tools

**`performance/`** - Performance Metrics
- Profiling infrastructure
- AST performance tracking

**`shims/`** - Compatibility Shims
- Optional dependency handling
- JSON library shim

## Module Dependencies

**Dependency Flow** (→ = depends on):
```
codegen → standardizers → semantic → analysis → ast → memory
   ↓           ↓             ↓          ↓        ↓
frontend → parser → lexer → common → utilities
```

**Key Principles**:
- Lower layers don't depend on upper layers
- `common/` and `utilities/` are leaf dependencies
- `memory/` provides foundation for `ast/`
- `frontend/` coordinates but doesn't implement algorithms

## File Organization

**File Size Targets**:
- Modules: <500 lines (hard limit 1000)
- Functions: <50 lines (hard limit 100)
- When exceeded: Split using `.inc` files with facade module

**Naming Conventions**:
- Modules: `<subsystem>_<component>.f90`
- Split modules: `<module>.f90` + `<module>_part1.inc` + `<module>_part2.inc`
- Example: `codegen_expressions.f90` + `codegen_expressions_part1.inc`

**Code Organization**:
- One module per file
- Public interface at top
- Implementation follows
- Use `only:` in all `use` statements

## Public API

**Stable Entry Points**:
- `fortfront.f90` - Main library facade
- `transformation_api.f90` - High-level transformation API
- `frontend/` modules - Pipeline components

**Internal Use Only**:
- Most subsystem modules are internal
- Use facade modules for external integration
- See `docs/LIBRARY_USAGE.md` for API examples

## Build System

**Primary Build Tool**: fpm (Fortran Package Manager)

**Configuration**: `fpm.toml` at repository root

**Build Commands**:
```bash
fpm build              # Build library
make build             # Build via Makefile
```

**Dependencies**:
- `stdlib` - Fortran standard library
- `test-drive` - Testing framework (test-only)

## Testing

Tests are in `test/` directory (not `src/`), but organized by source subsystem:
- `test/lexer/` tests `src/lexer/`
- `test/parser/` tests `src/parser/`
- etc.

See `test/README.md` for testing policies.

## Documentation

Each subdirectory has a `README.md` with:
- Purpose and responsibility
- File index with descriptions
- Key concepts and patterns
- Dependencies

**Navigation**:
1. Start here (`src/README.md`) for overview
2. Read subsystem README for specific area
3. Check `docs/` for architecture documentation

## Getting Started

**Understanding the codebase**:
1. Read this README (overview)
2. Read `lexer/README.md` (start of pipeline)
3. Follow pipeline: parser → semantic → codegen
4. Read `ast/README.md` (central data structure)

**Adding features**:
1. Identify affected subsystems
2. Read relevant READMEs
3. Follow existing patterns
4. Add tests in `test/` directory

**Debugging issues**:
1. Enable tracing: `debug_trace` utilities
2. Print AST: `app/debug_ast.f90`
3. Check subsystem dependencies
4. Review architecture docs in `docs/`

## Key Design Decisions

For complete design patterns and implementation details, see [CLAUDE.md Architecture Overview](../CLAUDE.md#architecture-overview) and the following documentation:

- **Arena Allocation**: `memory/README.md`, `docs/archive/MEMORY_SAFETY_ANALYSIS.md`
- **Visitor Pattern**: `ast/traversal/README.md`
- **Pratt Parser**: `parser/expressions/README.md`, `docs/PRATT_PIPELINE_ARCHITECTURE.md`
- **Type Inference**: `semantic/README.md`, `docs/SEMANTIC_PIPELINE_ARCHITECTURE.md`
- **Monomorphization**: `standardizers/README.md`, `docs/MONOMORPHIZATION.md`
