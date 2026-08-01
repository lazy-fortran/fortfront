# CLAUDE.md

**Note:** This file contains fortfront-specific guidance. General Fortran/Git/fpm/GitHub rules are in the user's global CLAUDE.md (`~/.claude/CLAUDE.md`) and always apply.

## Instruction Precedence (fortfront)

- The user's global CLAUDE.md at `~/.claude/CLAUDE.md` defines general rules for Fortran, Git, fpm, and GitHub.
- This fortfront `CLAUDE.md` refines those rules for this repository and is the canonical policy document here.
- When this file disagrees with other project documentation, scripts, or comments in this repo, this file wins.
- `AGENTS.md` in this repository must be a symlink to this file so there is exactly one source of truth.

## What is fortfront?

Fortfront is a **Fortran frontend library** that parses and analyzes **both standard Fortran and Lazy Fortran**. It provides a complete AST, semantic analysis, and type inference infrastructure for building tools like:

- **Linters and formatters** (fluff)
- **Compilers** (frontend input for downstream LIRIC lowering in `ffc`)
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

### CRITICAL: Zero Duplication Policy - MANDATORY ENFORCEMENT
**ONE canonical example, many references. NO DUPLICATION EVER.**

This is not a suggestion - it is an **absolute requirement**. Violations block PRs and must be fixed immediately.

### EXCEPTION: Unit Tests vs Integration Tests - BOTH ARE REQUIRED

**Unit tests with inline code are ENCOURAGED**. The zero-duplication policy applies to **END-TO-END tests ONLY**.

**Test Hierarchy (ALL levels required)**:
1. **Unit Tests** - Test individual functions/modules in isolation
   - ✅ **Inline code ALLOWED and ENCOURAGED** for small, focused test inputs
   - ✅ Test one function, one module, one specific code path
   - ✅ Fast, targeted, specific to the unit under test
   - Example: Testing a parser function with a 2-line input string

2. **Integration Tests** - Test interactions between components
   - ⚠️ **Use judgment**: Small inline snippets OK, large programs use examples/
   - Test multiple components working together
   - Example: Parser + semantic analyzer working on a small construct

3. **End-to-End Tests** - Test complete pipeline with realistic programs
   - ❌ **MUST use examples/**: These test complete transformation workflows
   - ❌ **NO inline code**: Full programs belong in examples/
   - Test: lexer → parser → semantic → codegen → compile → run
   - Example: Full lazy Fortran program → standardized Fortran output

**Decision Tree**:
```
Is this testing a SINGLE UNIT (function/module)?
├─ YES → Unit test with inline code is PERFECT ✅
└─ NO → Is it testing a complete program transformation?
    ├─ YES → MUST use examples/ (end-to-end) ❌ inline
    └─ NO → Integration test: use judgment (prefer examples/ for >10 lines)
```

### Directory Structure (STRICT)
```
examples/
├── f90/          # Standard Fortran examples (canonical source of truth)
│   ├── *.f90     # Round-trip validation examples
│   ├── issue_NNNN_description.f90  # Issue-specific standard Fortran
│   └── feature_name.f90            # Feature demonstrations
│
└── lf/           # Lazy Fortran examples (canonical source of truth)
    ├── *.lf      # Type inference and standardization examples
    ├── issue_NNNN_description.lf   # Issue-specific lazy Fortran
    └── feature_name.lf             # Feature demonstrations

test/
├── api/          # API tests (MUST use read_example)
├── ast/          # AST tests (MUST use read_example)
├── analysis/     # Analysis tests (MUST use read_example)
├── codegen/      # Codegen tests (MUST use read_example)
├── integration/  # Integration tests (MUST use read_example)
│   ├── issue_tests/      # Issue-specific test logic
│   ├── core_features/    # Core feature test logic
│   └── array_tests/      # Array feature test logic
└── *.f90         # All test files REFERENCE examples/, NEVER inline code
```

### Mandatory Rules - ZERO TOLERANCE

#### 1. **Examples are canonical sources (ENFORCED)**
   - `examples/` contains THE ONLY definitive example code
   - Examples demonstrate features, edge cases, and issue resolutions
   - Named descriptively: `generic_functions.lf`, `array_syntax.lf`, NOT `test_*.lf`
   - Issue demonstrations: `issue_NNNN_description.lf` (keep issue number for traceability)
   - Examples are documentation AND test inputs - dual purpose by design

#### 2. **End-to-End Tests MUST reference examples (ZERO INLINE CODE for full programs)**
   - **ABSOLUTELY FORBIDDEN in end-to-end tests**: Full program inline code
   - **ABSOLUTELY FORBIDDEN in end-to-end tests**: String concatenation with `new_line('a')` for complete programs
   - **ALLOWED in unit tests**: Small, focused inline code for testing individual functions
   - **REQUIRED for end-to-end**: Use `call read_example('examples/lf/file.lf', source)` pattern
   - **REQUIRED for end-to-end**: Use `call read_example('examples/f90/file.f90', source)` pattern
   - This prevents drift: examples and tests stay synchronized automatically

   **When to use inline code**:
   - ✅ Unit test: Testing `parse_expression()` with `"x + 5"` - PERFECT
   - ✅ Unit test: Testing `infer_type()` with `"val = 42"` - PERFECT
   - ❌ End-to-end: Full program with functions, calls, I/O - USE examples/

#### 3. **When you see inline code - EVALUATE THEN ACT**
   - **IF** you see inline code → **STOP and EVALUATE**
   - **ASK**: Is this a unit test or end-to-end test?

   **If UNIT TEST** (testing single function/module):
   - ✅ Inline code is FINE - keep it
   - ✅ This is the PREFERRED pattern for unit tests
   - Ensure it's focused and small (1-5 lines typical)

   **If END-TO-END TEST** (testing complete transformation):
   - ❌ **EXTRACT** the inline code to `examples/lf/` or `examples/f90/`
   - **NAME** descriptively based on what it demonstrates
   - **UPDATE** the test to use `read_example()`
   - **VERIFY** the test still passes
   - **COMMIT** with message: `refactor: extract end-to-end code to examples/ (fixes #NNNN)`
   - This is **MANDATORY** for end-to-end tests

#### 4. **Adding new examples (CORRECT PATTERN)**
   - Place in appropriate subdirectory: `examples/f90/` or `examples/lf/`
   - Use descriptive name reflecting what it demonstrates
   - Keep issue numbers for traceability: `issue_1234_array_bounds.lf`
   - One example file per feature/issue/edge case
   - Never inline the same code in multiple places

#### 5. **Adding new tests (CORRECT PATTERNS)**

   **For UNIT TESTS** (testing individual functions) - ✅ INLINE CODE PREFERRED:
   ```fortran
   ! Testing a parser function - inline code is PERFECT
   program test_parse_assignment
       use parser, only: parse_assignment
       implicit none
       type(ast_node_t) :: node

       node = parse_assignment("x = 42")
       call assert_equal(node%variable_name, "x")
       call assert_equal(node%value, 42)
   end program
   ```

   **For END-TO-END TESTS** - ❌ MUST USE EXAMPLES:

   **WRONG** (will be rejected for end-to-end):
   ```fortran
   source = 'function square(x)' // new_line('a') // &
            '    result = x * x' // new_line('a') // &
            'end function' // new_line('a') // &
            'val = 5' // new_line('a') // &
            'print *, square(val)'
   call transform_lazy_fortran_string(source, output, errors)
   ```

   **CORRECT** (for end-to-end):
   ```fortran
   ! 1. Create examples/lf/square_function.lf with the code
   ! 2. Reference it in test:
   call read_example('examples/lf/square_function.lf', source)
   call transform_lazy_fortran_string(source, output, errors)
   ```

   **Process for new tests**:
   1. **DECIDE**: Unit test or end-to-end test?
   2. **Unit test**: Write inline code directly - fast and focused
   3. **End-to-end test**: Create example file FIRST, then reference it
   4. Never duplicate examples - if similar code exists, reuse it

#### 6. **Deduplication enforcement (CI VALIDATES)**
   - Before ANY commit touching examples/ or test/:
     - **VERIFY**: End-to-end tests use `read_example()` pattern (no full programs inline)
     - **VERIFY**: Unit tests are focused and small (inline code OK here)
     - **VERIFY**: No .lf or .f90 files in `test/` directories (except test logic)
     - **VERIFY**: Large programs (>10 lines) are in examples/, not inline
   - CI MUST fail on end-to-end violations
   - Pre-commit hooks warn about large inline code blocks
   - Pull requests with full programs inline are REJECTED

### Rationale (WHY THIS MATTERS)

**Maintenance Crisis Avoided**:
- 161 test files had END-TO-END inline code (discovered in playtesting session #3)
- 3,000-5,000 lines of duplicated FULL PROGRAMS
- Every code pattern change required updating 161 string literals
- Refactoring was BLOCKED by this duplication
- **Note**: These were end-to-end tests that SHOULD have been using examples/

**Benefits of Unit Tests with Inline Code**:
- **Fast**: No file I/O overhead
- **Focused**: Test exactly one thing with minimal input
- **Clear**: Input visible right in the test
- **Maintainable**: Change one test without affecting others

**Benefits of End-to-End Tests Using Examples**:
- **Single source of truth**: Change example once, all tests automatically updated
- **No drift**: Tests always use current example code - impossible to get out of sync
- **Clear purpose**: examples/ = documentation, test/ = validation logic
- **Discoverability**: Examples are visible in examples/ directory, not buried in test strings
- **Reusability**: Multiple tests can reference the same example
- **Maintainability**: Refactoring is no longer blocked by inline code

**Test Pyramid - All Levels Required**:
```
        /\
       /E2E\      ← Few end-to-end tests (use examples/)
      /------\
     /  INT   \   ← More integration tests (use judgment)
    /----------\
   /   UNIT     \ ← Many unit tests (inline code preferred)
  /--------------\
```

**Code Review Efficiency**:
- Unit test: "tests parse_expression() with 'x + 5'" - clear and focused ✅
- End-to-end: "uses examples/lf/feature.lf" - clear intent ✅
- End-to-end inline: 20 lines of concatenated string literal - REJECTED ❌

### Example Migration (TEMPLATES)

#### Migrating END-TO-END Tests (REQUIRED)

**BEFORE** (WRONG - will be rejected for end-to-end):
```fortran
program test_function_inference_e2e
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source

    ! Full program inline - this is END-TO-END test
    source = 'function square(x)' // new_line('a') // &
             '    result = x * x' // new_line('a') // &
             'end function' // new_line('a') // &
             'val = 5' // new_line('a') // &
             'squared = square(val)'

    call transform_lazy_fortran_string(source, output, errors)
    ! ... assertions on full output ...
end program
```

**AFTER** (CORRECT - for end-to-end):
```fortran
program test_function_inference_e2e
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source

    call read_example('examples/lf/square_integer_inference.lf', source)
    call transform_lazy_fortran_string(source, output, errors)
    ! ... assertions on full output ...
end program
```

**NEW FILE** `examples/lf/square_integer_inference.lf`:
```fortran
function square(x)
    result = x * x
end function

val = 5
squared = square(val)
```

#### Unit Tests (NO MIGRATION NEEDED)

**CORRECT** (unit test - inline code is PERFECT):
```fortran
program test_type_inference_unit
    use semantic_analyzer, only: infer_type
    implicit none
    type(mono_type_t) :: result_type

    ! Unit test - testing ONE function with small input
    result_type = infer_type("x = 42")
    call assert_equal(result_type%kind, TYPE_INTEGER)
end program
```

**This is the PREFERRED pattern for unit tests** - do not extract to examples!

### Migration Status & Enforcement

**Issue #1975 - Test Deduplication Migration**

**Status: enforced.** `make check-duplication` reports 0 violations; some tests remain in the advisory warning band (6-15 inline `new_line('a')` occurrences), which the gate reports but does not fail on. End-to-end tests reference `examples/`; unit tests keep small inline inputs as described above.

**CI Enforcement** (ACTIVE — blocking since issue #2910):
- Check script: `scripts/check_test_duplication.py`
- Make target: `make check-duplication` (fails the build on violations)
- Negative control: `make check-duplication-gate` proves the gate can still fail
- CI workflow: `.github/workflows/ci.yml`

Keep it at zero: new end-to-end tests must use `read_example()` rather than inline full programs.

**How to Check Status**:
```bash
make check-duplication  # Shows current violations and warnings
```

### Helper Utilities

**Reading examples in tests**:
```fortran
! Already available in many test files:
subroutine read_example(filepath, content)
    character(len=*), intent(in) :: filepath
    character(len=:), allocatable, intent(out) :: content
    ! Reads file into allocatable string
end subroutine
```

**Finding inline code violations**:
```bash
# Find test files with inline code:
grep -r "new_line('a')" test/ | grep -v read_example

# Count violations:
find test -name "*.f90" -exec grep -l "new_line('a')" {} \; | wc -l
```

### Compliance Check

Before ANY commit or PR:
- [ ] **Unit tests**: Small, focused inline code is ENCOURAGED ✅
- [ ] **End-to-end tests**: MUST use `read_example()` for full programs
- [ ] All example programs in `examples/f90/` or `examples/lf/`
- [ ] No full programs inline (>10 lines with functions/modules)
- [ ] Example names are descriptive (feature-based, not test-based)
- [ ] CI passes (validates end-to-end use examples/)
- [ ] Test pyramid maintained: Many unit tests, some integration, few end-to-end

## Architecture Overview

### Pipeline Stages

Fortfront processes **both standard and lazy Fortran** through a multi-stage pipeline:

1. **Lexing** (`src/lexer/`) - Tokenize source text
2. **Parsing** (`src/parser/`) - Build the AST (Abstract Syntax Tree) directly from tokens
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
- See `docs/architecture/MONOMORPHIZATION.md` for detailed design
- **Not applicable to standard Fortran** - only for lazy Fortran type inference

### Module Organization

```
src/
├── analysis/        # Call graph, variable usage (foundation for tools)
├── ast/            # AST node types, arena, traversal (for both .f90 and .lf)
├── codegen/        # Standard Fortran emission (NOT backend IR)
├── common/         # Shared utilities (identifiers, UIDs)
├── cst/            # Lexical trivia (comments, whitespace) attached to AST nodes
├── frontend/       # High-level pipeline orchestration
├── interfaces/     # C API bindings (for non-Fortran tool integration)
├── lexer/          # Tokenization (handles both standard and lazy Fortran)
├── parser/         # Tokens -> AST transformation (unified parser)
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
- **Compilers (ffc):** Parse → AST → semantic → (ffc lowers to LIRIC)
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

**Performance Considerations**
- Stack usage: arena-based allocation keeps stack pressure low
- Test target `make test-small-stack` simulates Windows stack limits (1-2 MB)
- Large programs may need heap-based arenas

## Build & Test (fortfront-specific)

### Commands
```bash
# Standard fpm commands (see user CLAUDE.md for general usage)
fpm build
fpm test

# Fortfront-specific targets
make test-small-stack TEST_STACK_KB=1024  # Simulate Windows stack limits
make check-duplication                     # Validate zero-duplication policy

# Run CLI
./build/gfortran_<hash>/app/fortfront input.lf > output.f90
echo "x = 5" | ./build/gfortran_<hash>/app/fortfront > output.f90

# Format code
fprettify <file.f90>
```

### Critical Rules (fortfront-specific)
- **NEVER** pass `-j` flag to `make` or `fpm` - fpm manages parallelism automatically
- **NEVER** add custom compiler flags (e.g., `-cpp`, `-fmax-stack-var-size`) - use defaults
- Tests MUST reference `examples/` (see Examples & Tests Organization)
- Use `transform_lazy_fortran_string()` API for transformation tests
- CI runs on Linux AND Windows (includes stdin piping scenarios)
- If module resolution issues occur, run `make clean`

### Build Configuration
- `fpm.toml` - Package manifest
- `auto-executables = false` - Only explicit `[[executable]]` entries built
- `auto-tests = true` - All `test/*.f90` discovered automatically
- Depends on `stdlib` (Fortran standard library)

## Documentation & Navigation

### Finding Your Way Around

**Quick Navigation by Task**:
- **Understanding the pipeline**: `src/README.md` → pipeline diagram
- **Adding parser features**: `src/parser/README.md` → relevant subdir README
- **Type inference work**: `src/semantic/README.md` → `semantic/analyzers/README.md`
- **AST modifications**: `src/ast/README.md` → `ast/nodes/README.md`
- **Debugging issues**: `src/<subsystem>/README.md` → check dependencies

**Directory README Coverage** (All directories documented):

Core Pipeline:
- `src/README.md` - Overall architecture, pipeline stages, dependency flow
- `src/lexer/README.md` - Tokenization
- `src/parser/README.md` - Parsing (+ subdirs: core, declarations, expressions, statements, procedures, control_flow)
- `src/semantic/README.md` - Type inference and validation (+ subdirs: analyzers, types)
- `src/codegen/README.md` - Standard Fortran emission

AST Infrastructure:
- `src/ast/README.md` - AST overview (+ subdirs: nodes, traversal, arena, factory)

Supporting Subsystems:
- `src/analysis/README.md` - Call graph, variable usage
- `src/standardizers/README.md` - AST standardization passes
- `src/frontend/README.md` - Pipeline orchestration

Utilities:
- `src/memory/README.md` - Arena allocators
- `src/common/README.md` - Shared utilities (identifiers, UIDs)
- `src/utilities/README.md` - String handling, debug tracing
- `src/interfaces/README.md` - C API bindings
- `src/cst/README.md` - Lexical trivia surface

Application & Tests:
- `app/README.md` - CLI application
- `test/README.md` - Test suite organization, zero-duplication policy
- `examples/README.md` - Canonical example files

**Navigation Decision Tree**:
```
What are you doing?
├─ Adding a feature
│  ├─ Identify affected subsystems (lexer, parser, semantic, codegen)
│  ├─ Read src/<subsystem>/README.md
│  ├─ Check subdirectory READMEs for specific area
│  └─ Follow patterns documented in README
│
├─ Debugging an issue
│  ├─ Start at src/<subsystem>/README.md
│  ├─ Check "Dependencies" section
│  ├─ Follow dependency chain READMEs
│  └─ Review docs/ for implementation details
│
├─ Understanding architecture
│  ├─ Read src/README.md (pipeline overview)
│  ├─ Read docs/guides/LIBRARY_USAGE.md (API guide)
│  ├─ Follow pipeline: lexer → parser → semantic → codegen
│  └─ Check subsystem READMEs for details
│
└─ Finding specific code
   ├─ Guess subsystem, read its README.md
   ├─ Check "File Index" table in README
   ├─ Navigate to specific file
   └─ If wrong subsystem, check "Dependencies" for hints
```

### Directory README Structure

**EVERY directory** has a README.md following **Chromium directory documentation pattern**:

**Standard Sections**:
- **Purpose**: What the directory contains and its role in the system
- **File Index**: Table listing all files with descriptions
- **Key Concepts**: Important patterns and design decisions
- **Dependencies**: Module and external dependencies

**When making changes**:
- Update the relevant directory README.md when adding/removing files
- Update file descriptions when changing module purpose
- Ensure README stays synchronized with actual code
- If adding a new directory, create a README following the pattern

**README Maintenance**:
- Part of code review - check README changes when files added/removed
- READMEs are code documentation - keep them accurate
- File index tables must match actual directory contents
- Update before committing structural changes

### Key Documentation

#### Essential Docs (read these first)
- `docs/architecture/MONOMORPHIZATION.md` - Type inference and specialization strategy
- `docs/guides/LIBRARY_USAGE.md` - API usage examples for tool developers
- `docs/guides/TYPE_SAFETY_GUIDE.md` - Type system implementation details

#### Implementation Guides
- `docs/architecture/SEMANTIC_PIPELINE_ARCHITECTURE.md` - Semantic analysis design
- `docs/architecture/PRATT_PIPELINE_ARCHITECTURE.md` - Parser implementation (Pratt parsing)
- `docs/guides/MIXED_CONSTRUCTS_GUIDE.md` - Handling `.lf` files with embedded Fortran
- `docs/reference/NODE_TYPE_IDENTIFICATION.md` - AST node type patterns
- `docs/guides/CHARACTER_TYPE_GUIDE.md` - String handling in Fortran

#### Reference
- `docs/archive/AST_MIGRATION.md` - AST architecture evolution
- `docs/archive/MEMORY_SAFETY_ANALYSIS.md` - Historical memory safety notes
- `docs/archive/PARSE_DECLARATION_REFACTORING.md` - Historical parser refactoring notes
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
5. Use external profilers for detailed timing and allocation data
