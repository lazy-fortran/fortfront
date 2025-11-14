# Fortfront Directory Structure

This document provides a comprehensive overview of the fortfront codebase organization. Each directory now contains a README.md file with detailed information about its purpose, contents, and dependencies.

## Quick Reference

| Directory | Purpose | Key Components |
|-----------|---------|----------------|
| `src/` | Core compiler implementation | Lexer, parser, semantic, codegen |
| `app/` | CLI application | Command-line interface |
| `test/` | Test suite | Unit, integration, end-to-end tests |
| `examples/` | Example source files | `.lf` and `.f90` canonical examples |
| `docs/` | Technical documentation | Architecture, design, guides |
| `scripts/` | Build and validation scripts | CI utilities, duplication checker |
| `.github/workflows/` | CI/CD workflows | Automated testing and validation |

## Source Code (`src/`)

### Core Subsystems

**`src/lexer/`** - Tokenization
- Token stream generation from source text
- Keyword recognition, literal scanning
- Location tracking for error reporting

**`src/parser/`** - Parsing
- `parser/core/` - Parser infrastructure
- `parser/declarations/` - Declaration parsing
- `parser/expressions/` - Expression parsing (Pratt parser)
- `parser/statements/` - Statement parsing
- `parser/procedures/` - Procedure parsing
- `parser/control_flow/` - Control flow parsing

**`src/ast/`** - Abstract Syntax Tree
- `ast/nodes/` - AST node type definitions
- `ast/traversal/` - Visitor pattern implementation
- `ast/arena/` - Arena-based memory management
- `ast/factory/` - AST node creation factories

**`src/semantic/`** - Semantic Analysis
- `semantic/analyzers/` - Type inference and validation
- `semantic/types/` - Type system implementation
- Scope management, type checking, convergence

**`src/codegen/`** - Code Generation
- Emit standard Fortran from AST
- Declaration grouping, indentation
- Name mangling for monomorphization

**`src/analysis/`** - Program Analysis
- Call graph construction
- Variable usage tracking
- Procedure signature analysis

### Supporting Infrastructure

**`src/frontend/`** - Pipeline Orchestration
- High-level transformation API
- Mixed construct handling
- Program structure detection

**`src/standardizers/`** - AST Standardization
- Add inferred declarations
- Insert intent attributes
- Monomorphization implementation

**`src/memory/`** - Memory Management
- Arena allocators
- Compiler-wide allocation context

**`src/common/`** - Shared Utilities
- Identifier interning
- UID generation
- Attribute handling

**`src/utilities/`** - General Utilities
- String manipulation
- Debug tracing
- CLI environment

**`src/cst/`** - Concrete Syntax Tree
- Lossless source representation
- For formatters and refactoring tools

**`src/interfaces/`** - C Bindings
- C-compatible API
- For non-Fortran tool integration

**`src/performance/`** - Performance Metrics
- Profiling infrastructure
- AST performance tracking

**`src/shims/`** - Compatibility Shims
- Optional dependency handling
- JSON library shim

## Application (`app/`)

**CLI Application**
- Command-line interface for lazy Fortran transformation
- File I/O, stdin/stdout handling
- Error reporting

## Tests (`test/`)

**Test Organization**
- `test/lexer/` - Lexer tests
- `test/parser/` - Parser tests
- `test/semantic/` - Semantic analysis tests
- `test/codegen/` - Code generation tests
- `test/integration/` - Integration tests (organized by feature)
- `test/api/` - Public API tests
- `test/ast/` - AST tests
- `test/analysis/` - Analysis subsystem tests
- `test/system/` - System-level tests

**Test Policy**
- **Unit tests**: Inline code ENCOURAGED ✅
- **End-to-end tests**: MUST use `examples/` ❌ no full programs inline
- See `CLAUDE.md` for complete zero-duplication policy

## Examples (`examples/`)

**Canonical Source Files**
- `examples/f90/` - Standard Fortran examples (round-trip validation)
- `examples/lf/` - Lazy Fortran examples (transformation testing)
- `examples/hello/` - Simple hello world examples

**Purpose**: Dual-use as documentation and test inputs

## Documentation (`docs/`)

**Essential Documentation**
- `LIBRARY_USAGE.md` - API usage guide
- `MONOMORPHIZATION.md` - Type inference strategy
- `PRATT_PIPELINE_ARCHITECTURE.md` - Parser design
- `SEMANTIC_PIPELINE_ARCHITECTURE.md` - Semantic analysis
- `MEMORY_SAFETY_ANALYSIS.md` - Memory management

## Scripts (`scripts/`)

**Build and Validation**
- `build.sh` - Build wrapper
- `check_test_duplication.py` - Zero-duplication enforcer
- `run_gfortran_roundtrip.py` - Round-trip validator
- `with_timeout.sh` / `.ps1` - Timeout utilities

## CI/CD (`.github/workflows/`)

**Automated Testing**
- `ci.yml` - Main CI workflow
- Build, test, lint, duplication check
- Linux and Windows platforms

## Navigation Tips

**Finding Code**:
1. Start with subsystem README: `src/<subsystem>/README.md`
2. Check subdirectory READMEs for details
3. Use file tables in READMEs to locate specific functionality

**Understanding Architecture**:
1. Read `docs/LIBRARY_USAGE.md` for overview
2. Read subsystem READMEs for specific components
3. Check `docs/` for design documentation

**Adding Features**:
1. Identify affected subsystems
2. Read relevant subsystem READMEs
3. Follow patterns documented in READMEs
4. Add tests following test policy

**Debugging Issues**:
1. Check subsystem README for dependencies
2. Review related subsystem READMEs
3. Check `docs/` for implementation guides
4. Use debug utilities in `app/debug_ast.f90`

## README Locations

Every directory has a README.md:

```
src/README.md (create this for src overview)
src/analysis/README.md ✓
src/ast/README.md ✓
src/ast/nodes/README.md ✓
src/ast/traversal/README.md ✓
src/ast/arena/README.md ✓
src/ast/factory/README.md ✓
src/codegen/README.md ✓
src/common/README.md ✓
src/cst/README.md ✓
src/frontend/README.md ✓
src/interfaces/README.md ✓
src/lexer/README.md ✓
src/memory/README.md ✓
src/parser/README.md ✓
src/parser/core/README.md ✓
src/parser/declarations/README.md ✓
src/parser/expressions/README.md ✓
src/parser/statements/README.md ✓
src/parser/procedures/README.md ✓
src/parser/control_flow/README.md ✓
src/semantic/README.md ✓
src/semantic/analyzers/README.md ✓
src/semantic/types/README.md ✓
src/standardizers/README.md ✓
src/utilities/README.md ✓
src/performance/README.md ✓
src/shims/README.md ✓
app/README.md ✓
test/README.md ✓
examples/README.md ✓
docs/DIRECTORY_README.md ✓
scripts/README.md ✓
.github/workflows/README.md ✓
```

## Quick Start

**New to fortfront?**
1. Read `/home/ert/code/fortfront/README.md` (project overview)
2. Read `docs/LIBRARY_USAGE.md` (API guide)
3. Explore `examples/` (working code samples)
4. Read relevant subsystem READMEs

**Contributing?**
1. Read `CLAUDE.md` (development rules)
2. Read subsystem READMEs for areas you're modifying
3. Follow test policy in `test/README.md`
4. Check CI requirements in `.github/workflows/README.md`

**Building tools with fortfront?**
1. Read `docs/LIBRARY_USAGE.md` (API examples)
2. Read `docs/ECOSYSTEM.md` (integration patterns)
3. Read `src/interfaces/README.md` (C bindings)
4. Explore `examples/` for test inputs
