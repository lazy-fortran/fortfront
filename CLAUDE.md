# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is fortfront?

Fortfront is a **Fortran frontend library** that parses and analyzes **both standard Fortran and Lazy Fortran**. It provides a complete AST, semantic analysis, and type inference infrastructure for building tools like:

- **Linters and formatters** (fluff)
- **Compilers** (LLVM HLIR emission)
- **Static analyzers**
- **Language servers**
- **Code transformation tools**

**On its own**, fortfront can also **standardize Lazy Fortran** to standard Fortran via CLI and API.

**Lazy Fortran transformation example:**
```fortran
! Input: script.lf (lazy fortran - minimal syntax)
function add(a, b)
    add = a + b
end function
x = add(5, 3)

! Output: standard Fortran (inferred types, intents, structure)
program main
    implicit none
    integer :: x
contains
    integer function add(a, b)
        integer, intent(in) :: a, b
        add = a + b
    end function
end program
```

**Standard Fortran round-trip:** `.f90` → parse → AST → emit → `.f90` (validates correctness)

## Examples & Tests Organization

### CRITICAL: Zero Duplication Policy
**ONE canonical example, many references. NO DUPLICATION EVER.**

### Directory Structure
```
examples/
├── f90/          # Standard Fortran examples (for round-trip validation)
│   └── *.f90     # Tests that parser handles standard Fortran correctly
└── lf/           # Lazy Fortran examples (for transformation testing)
    └── *.lf      # Tests that type inference and standardization work

test/
├── snapshots/
│   └── cases/    # MUST be empty of .lf files (all moved to examples/)
└── *.f90         # Test files that REFERENCE examples/, never inline duplicate content
```

### Rules

1. **Examples are canonical sources**
   - `examples/` contains THE definitive example code
   - Examples demonstrate features, edge cases, and issue resolutions
   - Named descriptively: `generic_functions.lf`, `array_syntax.lf`, NOT `test_*.lf`
   - Issue demonstrations: `issue_NNNN_description.lf` → rename to `feature_description.lf`

2. **Tests reference examples**
   - Tests in `test/` MUST NOT duplicate example content inline
   - Tests should read from `examples/` files when testing parsing/transformation
   - If a test generates Lazy Fortran code, extract it to `examples/` and reference it
   - This prevents drift: examples and tests stay synchronized automatically

3. **Deduplication enforcement**
   - Before ANY commit touching examples/ or test/:
     - Run deduplication audit (see issue #1867)
     - Verify zero .lf files remain in `test/snapshots/cases/`
     - Verify no string literals in tests duplicate example file content
   - CI MUST validate: no duplicates exist

4. **Adding new examples**
   - Place in appropriate subdirectory: `examples/f90/` or `examples/lf/`
   - Use descriptive name reflecting what it demonstrates
   - Update tests to reference the new example file
   - Never inline the same code in both places

5. **Adding new tests**
   - If test needs Lazy Fortran input: create example file first, reference it
   - If test needs standard Fortran: create example file first, reference it
   - Never duplicate existing examples/ content

### Rationale
- **Single source of truth**: Examples are documentation AND test inputs
- **No drift**: Tests always use current example code
- **Clear purpose**: examples/ = documentation, test/ = validation logic
- **Maintainability**: Change example once, all tests automatically updated
- **Repository hygiene**: Obvious what each directory contains

### Migration Status
See issue #1867 for ongoing reorganization work.

## Architecture Overview

### Pipeline Stages

Fortfront processes **both standard and lazy Fortran** through a multi-stage pipeline:

1. **Lexing** (`src/lexer/`) - Tokenize source text
2. **Parsing** (`src/parser/`) - Build CST (Concrete Syntax Tree), then AST (Abstract Syntax Tree)
3. **Semantic Analysis** (`src/semantic/`) - Type inference, scope resolution, validation
4. **Program Analysis** (`src/analysis/`) - Call graph, variable usage tracking
5. **Code Generation** (`src/codegen/`) - Emit standard Fortran

**For Lazy Fortran:** All stages run, type inference fills in missing information
**For Standard Fortran:** Parse → semantic validation (tools like linters/compilers use the AST)

**CLI entry point:** `app/fortfront.f90` (lazy fortran standardization)
**Library API:** `src/fortfront.f90` (facade module for tool builders)

### Core Subsystems

**AST Management** (`src/ast/`)
- Arena-based allocation for AST nodes (no manual deallocation)
- Node types: `ast_nodes_core`, `ast_nodes_procedure`, `ast_nodes_control`, `ast_nodes_loops`, etc.
- **CRITICAL:** AST nodes MUST NOT be copied - use visitor pattern only
- Safe access: `visit_node_at()`, `ast_traversal` utilities

**Semantic Analysis** (`src/semantic/`)
- **Type System** (`types/`) - `mono_type_t`, `poly_type_t`, type inference
- **Analyzers** (`analyzers/`) - Type inference, scope resolution, validation
- **Scope Manager** - Symbol table, variable declarations, nested scopes
- **Purpose:** Fill in missing type information (lazy Fortran) or validate existing types (standard Fortran)

**Program Analysis** (`src/analysis/`)
- **Call Graph** - Track all function/subroutine calls, detect unused procedures, find cycles
- **Variable Usage** - Track which variables are referenced where
- **Purpose:** Basic program structure analysis to support type inference and provide foundation for tools
- **Key distinction from semantic:** Operates on COMPLETE, type-checked AST; provides program-wide structure info

**Memory Management** (`src/memory/`)
- `arena_memory` - General-purpose arena allocator
- `compiler_arena` - Compiler-wide allocation context
- Stack-like allocation, automatic cleanup on scope exit

**Frontend** (`src/frontend/`)
- High-level transformation orchestration
- Program structure detection (wrap bare statements in `program main` for lazy Fortran)
- Mixed construct handling (`.lf` files with embedded standard Fortran)

### Key Design Patterns

**Arena Allocation**
- All AST nodes allocated in arena
- No manual `deallocate` - arena cleanup handles everything
- Use `allocatable`, avoid pointers

**Visitor Pattern for AST**
- Traverse AST with `visit_node_at(arena, index, visitor_callback)`
- Never copy nodes - visitor receives node reference
- See `src/ast/traversal/ast_visitor.f90`

**Semantic Context**
- `semantic_context_t` holds scope stack, type environment, identifier table
- Created once, threaded through analysis passes
- Incremental type refinement (multiple passes until convergence)

**Monomorphization Strategy (Lazy Fortran Only)**
- Single-file: fortfront generates all type specializations used in file
- Cross-module: package managers orchestrate using fortfront API
- Uses Fortran generic interfaces (standard Fortran, no extensions)
- See `docs/MONOMORPHIZATION.md` for detailed design
- **Not applicable to standard Fortran** - only for lazy Fortran type inference

### Module Organization

```
src/
├── analysis/        # Call graph, variable usage (foundation for tools)
├── ast/            # AST node types, arena, traversal (for both .f90 and .lf)
├── codegen/        # Standard Fortran emission (NOT LLVM - that's ffc's job)
├── common/         # Shared utilities (identifiers, UIDs)
├── cst/            # Concrete syntax tree (preserves all source details)
├── frontend/       # High-level pipeline orchestration
├── interfaces/     # C API bindings (for non-Fortran tool integration)
├── lexer/          # Tokenization (handles both standard and lazy Fortran)
├── parser/         # CST → AST transformation (unified parser)
├── semantic/       # Type inference + validation (inference for .lf, validation for .f90)
├── standardizers/  # Lazy Fortran → standard Fortran transformation passes
└── utilities/      # String handling, debug tracing, CLI

app/
└── fortfront.f90   # CLI driver (lazy fortran standardization)

test/
└── *.f90          # Test files that REFERENCE examples/

examples/
├── f90/           # Standard Fortran (round-trip validation)
└── lf/            # Lazy Fortran (transformation testing)
```

### Use Cases

**1. As a Library (Primary Use Case)**
- **Linters/Formatters (fluff):** Parse → AST → (fluff does analysis) → report/format
- **Compilers (ffc):** Parse → AST → semantic → (ffc emits LLVM HLIR)
- **Build Tools (fortrun):** Parse → AST → semantic → cross-module inference
- **Language Servers:** Parse → AST → semantic context → provide completions/diagnostics
- **Fortfront provides: Lexer, Parser, AST, Type Inference, Basic Analysis**
- **Tools build on top: CFG, dataflow, optimization, code emission**
- **All tools work with BOTH standard and lazy Fortran**

**2. As a CLI (Standardization)**
- Transform lazy Fortran (`.lf`) to standard Fortran (`.f90`)
- Infer types, add declarations, fix intents, wrap in program structure
- `fortfront input.lf > output.f90`

**3. Round-Trip Validation**
- Parse standard Fortran → AST → emit standard Fortran
- Validates parser correctness
- Examples in `examples/f90/` test this capability

### Important Implementation Notes

**Type Inference (Lazy Fortran Only)**
- Infers from literals: `x = 5` → `integer`
- Infers from call sites: `add(5, 3)` → function parameters are `integer`
- Multiple passes until types converge
- See `src/semantic/analyzers/semantic_analyzer_base.f90`

**Type Validation (Standard Fortran)**
- Verifies declared types match usage
- Checks type compatibility in expressions
- Ensures procedure calls have correct argument types

**Name Mangling (for monomorphization)**
- Format: `<name>__<kind1>_<kind2>`
- Example: `add__i32_i32` for `integer(4) add(integer(4), integer(4))`
- Deterministic to avoid collisions

**Fortran Compliance**
- Output is always standard-conforming Fortran 2018
- No language extensions in emitted code
- Uses `implicit none`, explicit `intent()`, proper declarations

**Performance Considerations**
- Stack usage: arena-based allocation keeps stack pressure low
- Test target `make test-small-stack` simulates Windows stack limits (1-2 MB)
- Large programs may need heap-based arenas

## Build & Test

### Common Commands
```bash
# Build the project
fpm build
make          # Convenience wrapper

# Run all tests
fpm test
make test     # Convenience wrapper

# Run tests with small stack (simulate Windows)
make test-small-stack TEST_STACK_KB=1024

# Clean build artifacts
fpm clean --all
make clean

# Run CLI
./build/gfortran_<hash>/app/fortfront input.lf > output.f90
echo "x = 5" | ./build/gfortran_<hash>/app/fortfront > output.f90

# Run specific test
fpm test <test_name>

# Format code
fprettify --indent 4 --line-length 88 <file.f90>
```

### Testing Strategy
- Unit tests in `test/` files - test individual modules/functions
- Integration tests - full pipeline transformations
- Tests MUST reference `examples/` files, not inline duplicate code
- Use `transform_lazy_fortran_string()` API for testing transformations
- Behavioral tests preferred: input → transformation → verify output
- Keep tests fast (≤120s each)

### Build Configuration
- `fpm.toml` - Package manifest
- `auto-executables = false` - Only explicit `[[executable]]` entries built
- `auto-tests = true` - All `test/*.f90` discovered automatically
- Depends on `stdlib` (Fortran standard library)
- Free-form source, implicit typing disabled

## Fortran Standards
- Modern Fortran (2018+)
- Use `allocatable`, avoid pointers unless required
- All procedures have explicit `intent(in|out|inout)`
- Mark `pure`/`elemental` where appropriate
- Derived types named `<name>_t`
- Use `use <module>, only:` statements

## Git Workflow
- SSH only, no HTTPS
- Stage files explicitly: `git add path/to/file`, NEVER `git add .` or `git add -A`
- No emojis in commits, PRs, or issues
- CI must pass before merge
- Run `fpm test` locally before creating/updating PRs

## GitHub CLI Usage
- List issues: `gh issue list --state open --limit 500`
- List PRs: `gh pr list --state open --limit 500`
- Edit issue body: `gh issue edit <number> --body-file <file.md>`
- Create PR: `gh pr create --title "<title>" --body-file <file.md> --base main --head <branch>`
- Check CI: `gh pr checks <number> --watch`

## Code Quality
- Modules <500 lines (hard limit 1000)
- Functions <50 lines (hard limit 100)
- No stubs, placeholders, or commented-out code
- No hardcoded secrets/keys
- Remove dead code immediately
- Self-documenting code; comments for non-obvious intent only

## Documentation
- Keep in `docs/` directory
- No random markdown files in working directory
- Update docs when behavior changes
- Examples in `examples/`, not inline in docs

## Licensing
- Research-first: copy ideas, not lines
- Verify licenses: prefer MIT/BSD/Apache-2.0
- Preserve notices when required

## Key Documentation

### Essential Docs (read these first)
- `docs/MONOMORPHIZATION.md` - Type inference and specialization strategy
- `docs/LIBRARY_USAGE.md` - API usage examples for tool developers
- `docs/MEMORY_SAFETY_ANALYSIS.md` - Arena allocation and AST safety rules
- `docs/TYPE_SAFETY_GUIDE.md` - Type system implementation details

### Implementation Guides
- `docs/SEMANTIC_PIPELINE_ARCHITECTURE.md` - Semantic analysis design
- `docs/PRATT_PIPELINE_ARCHITECTURE.md` - Parser implementation (Pratt parsing)
- `docs/MIXED_CONSTRUCTS_GUIDE.md` - Handling `.lf` files with embedded Fortran
- `docs/NODE_TYPE_IDENTIFICATION.md` - AST node type patterns
- `docs/CHARACTER_TYPE_GUIDE.md` - String handling in Fortran

### Reference
- `docs/AST_MIGRATION.md` - AST architecture evolution
- `docs/PARSE_DECLARATION_REFACTORING.md` - Parser refactoring history
- `docs/ECOSYSTEM.md` - Integration with fortrun and package managers

## Common Development Workflows

### Adding a New AST Node Type
1. Define node in appropriate `src/ast/nodes/ast_nodes_*.f90`
2. Add visitor support in `src/ast/traversal/ast_visitor.f90`
3. Update parser to create the node
4. Add semantic analysis for the node
5. Add codegen emission for the node
6. Add tests in `test/` (reference examples)
7. Add example in `examples/lf/` or `examples/f90/`

### Adding a New Type Inference Rule
1. Identify where inference occurs (assignment, call, expression)
2. Modify appropriate semantic analyzer in `src/semantic/analyzers/`
3. Update `semantic_context_t` if new type information needed
4. Add convergence logic if multi-pass required
5. Add tests covering the new inference pattern

### Debugging Type Inference Issues
1. Enable tracing: `fortfront --trace input.lf`
2. Check trace output in `fortfront_trace.log`
3. Inspect semantic context state at each pass
4. Verify call graph captures all call sites correctly
5. Check type unification and constraint solving

### Performance Investigation
1. Profile with `gprof` or `perf`
2. Check arena allocation patterns (excessive growth?)
3. Review AST traversal counts (redundant passes?)
4. Measure stack usage: `ulimit -s 1024; fpm test`
5. See `src/performance/ast_performance.f90` for metrics
