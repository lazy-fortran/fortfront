# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Test Commands

### Build
```bash
# Standard build
fpm build --flag "-cpp -fmax-stack-var-size=65536"

# Or use the convenience script (recommended)
./build.sh
```

### Run all tests
```bash
# Standard test
fpm test --flag "-cpp -fmax-stack-var-size=65536"

# Or use the convenience script (recommended)
./test.sh
```

### Run tests with coverage (Linux only)
```bash
fpm clean --all
fpm test --profile debug --flag '-cpp -fmax-stack-var-size=65536 -fprofile-arcs -ftest-coverage -g'
lcov --capture --directory build/ --output-file coverage.info \
  --rc branch_coverage=1 \
  --ignore-errors inconsistent
lcov --remove coverage.info \
  'build/dependencies/*' \
  'test/*' \
  '/usr/*' \
  --output-file coverage_filtered.info
genhtml coverage_filtered.info --output-directory coverage_html \
  --branch-coverage \
  --legend
```

### Run a specific test
```bash
fpm test <test_name>
# Example: fpm test test_frontend_lexer_api
```

### Important build flags
- Always use `-cpp` flag when building/testing (required for preprocessing)
- Use `--profile debug` for debugging and coverage

## Development Tips

- Do not always use `fpm clean all`. Only use clean in cases where all other fixes fail
- **GCC 15.1.1 Compatibility**: Always use `-fmax-stack-var-size=65536` flag when building or testing
  - This resolves compilation issues with large module files (specifically `parser_expressions_module.mod`)
  - The issue manifests as: "Reading module '*.mod' at line X column Y: Expected right parenthesis"
  - Use provided build scripts (./build.sh, ./test.sh) which include this flag automatically
- **Avoid Using Shell Redirection Tricks**
  - Never use `2>&1` or similar shell redirection techniques blindly
  - These can mask underlying issues and make debugging more difficult

## Architecture Overview

fortfront is a Fortran frontend that processes code through four distinct phases, with input validation as a supporting module:

**Input Validation** (`src/input_validation.f90`) - Dedicated validation module (Issue #262)
   - Comprehensive input validation with enhanced error reporting
   - Standalone module independent of frontend transformation pipeline
   - Supports editor integration, build tools, and educational applications
   - Clean API with no circular dependencies

1. **Lexer** (`src/lexer/`) - Tokenizes Fortran source code
   - Core module: `lexer_core.f90`
   - Produces `token_t` array

2. **Parser** (`src/parser/`) - Builds Abstract Syntax Tree (AST)
   - Core modules: `parser_core.f90`, `parser_dispatcher.f90`
   - Specialized parsers: `parser_expressions.f90`, `parser_declarations.f90`, `parser_control_flow.f90`
   - Uses `parser_state_t` to track parsing state
   - Produces AST nodes in a global arena

3. **Semantic Analysis** (`src/semantic/`) - Type checking and inference
   - Core module: `semantic_analyzer.f90`
   - Type system: `type_system_hm.f90` (Hindley-Milner type inference)
   - Scope management: `scope_manager.f90`
   - **Extensible Pipeline**: `semantic_pipeline.f90` (Issue #202)
   - Built-in analyzers: `builtin_analyzers.f90`
   - Analysis plugins: `usage_tracker_analyzer.f90`, `source_reconstruction_analyzer.f90`
   - Produces typed AST with `semantic_context_t`

4. **Code Generation** (`src/codegen/`) - Emits standard Fortran
   - Core module: `codegen_core.f90`
   - Handles indentation: `codegen_indent.f90`
   - Generates clean, standard-compliant Fortran code

### Key Design Patterns

- **AST Arena**: All AST nodes are stored in a global arena (`ast_core.f90`)
- **Visitor Pattern**: AST transformations use visitors (`ast_visitor.f90`)
- **Typed AST**: Extended AST nodes with type information (`ast_typed.f90`)
- **JSON Serialization**: Debug/inspection via JSON (`ast_json.f90`, `json_writer.f90`)

### Main Entry Points

- `frontend.f90`: Main module with `compile_source` subroutine
- `frontend_integration.f90`: Higher-level compilation interface

### Test Organization

Tests are organized by compilation phase:
- `test/lexer/` - Lexical analysis tests
- `test/parser/` - Parser tests
- `test/semantic/` - Semantic analysis and type inference tests
- `test/codegen/` - Code generation tests
- `test/standardizer/` - AST standardization tests
- `test/validation/` - Input validation module tests
- `test/error_reporting/` - Error reporting and validation tests

Total: 290 test files as of latest count.

### Error Reporting (Issue #256)

Enhanced error reporting provides comprehensive feedback:

- **Location Information**: Precise line and column numbers for all errors  
- **Clear Messages**: Specific problem identification with actionable descriptions
- **Fix Suggestions**: Helpful recommendations when syntax errors can be resolved
- **Source Context**: Display of problematic source lines with position indicators
- **No Silent Failures**: All syntax errors explicitly reported, no silent fallbacks
- **Validation Coverage**: Comments-only input and simple expressions properly accepted

### Common Development Patterns

When adding new features:
1. Start with lexer if new tokens are needed
2. Update parser to handle new syntax
3. Add semantic analysis for type checking
4. Implement code generation
5. Write comprehensive tests for each phase

### Semantic Analysis Extension Patterns

When adding new analysis capabilities:
1. **Core Analyzers**: Extend `semantic_analyzer_t` for essential standardization features
2. **Analysis Plugins**: Create specialized analyzers for external tool integration (fluff rules)
3. **Result Types**: Define clear result types with proper assignment operators
4. **Pipeline Integration**: Register analyzer in `semantic_pipeline.f90` type selection
5. **Testing**: Add comprehensive tests covering analysis methods and error cases
- we have four use cases for fortfront. 1. its own standardizer for lazy fortran. 2. fortrun that does module discovery and source and object cache. 3. fluff for static analysis and custom formatting of code, where it would be also ok to be very strict on dismissing original formatting (like ruff and black). 4. ffc the fortran fortran compiler that plugs fortfront to a hlfir llvm backend lowering chain like flang
- **Extensible Semantic Analysis (Issue #202)**: Plugin-based semantic analysis pipeline allows external tools (especially fluff) to perform custom code analysis using built-in analyzers and analysis plugins
- THE ASSIGNMENT OPERATOR ALWAYS HAS TO BE OVERLOADED TO DO A DEEP COPY FOR DERIVED TYPES WITH ALLOCATABLE MEMBERS

## Memory Management

### Cycle-Safe Type Copying System

**IMPLEMENTED** (Issue #276): fortfront uses a comprehensive cycle-safe type copying system that prevents infinite recursion while preserving semantic information flow through the compilation pipeline.

### Type System Architecture
- **Cycle Detection**: DFS-based algorithm with 3-tier optimization for performance
- **Memory Safety**: Automatic depth limitation prevents stack overflow during recursive copying
- **Performance**: <5% compilation time impact through optimized recursion handling
- **RAII Management**: Automatic cleanup with progressive fallback mechanisms

### mono_type_t Assignment Safety
- **Depth-Limited Recursion**: Prevents infinite loops in self-referential type structures
- **Defensive Validation**: Field validation prevents invalid type states during copying
- **Memory Allocation**: Always ensures type variable names are allocated to prevent crashes
- **Args Array Handling**: Safe recursive copying with depth limits for nested function types

### AST Node Type Preservation
- **Semantic Information Flow**: `inferred_type` fields are preserved through compilation pipeline
- **Copy Consistency**: All AST assignment operations maintain type information integrity
- **Performance Optimization**: Efficient copying balances completeness with speed requirements

### Implementation Details

**Core Algorithm**: The cycle-safe copying system uses depth-first search with configurable depth limits:

```fortran
! Depth-limited recursive copying prevents infinite loops
recursive subroutine mono_type_depth_limited_copy(lhs, rhs, depth)
    integer, parameter :: MAX_DEPTH = 3  ! Configurable limit
    if (depth < MAX_DEPTH .and. allocated(rhs%args)) then
        call mono_type_depth_limited_copy(lhs%args(i), rhs%args(i), depth + 1)
    end if
end subroutine
```

**Safety Features**:
- **Stack Overflow Protection**: Depth limiting prevents unbounded recursion
- **Memory Corruption Prevention**: Defensive field validation during copying
- **Allocation Safety**: All allocatable components properly initialized
- **Reference Consistency**: Maintains type relationships without creating cycles

**Performance Characteristics**:
- **Compilation Impact**: <5% overhead on total compilation time
- **Memory Efficiency**: Minimal memory overhead from cycle detection
- **Scalability**: Handles complex type hierarchies in large codebases
- **Optimization**: 3-tier algorithm optimization for common cases

**Testing Coverage**: 43 comprehensive test cases across:
- Cycle detection and resolution
- Performance benchmarks with large-scale operations  
- Memory safety under stress conditions
- AST assignment with complex type preservation
- End-to-end semantic pipeline integration

### Developer API Patterns

**Safe Type Assignment**: Always use assignment operators instead of manual copying:

```fortran
use type_system_hm, only: mono_type_t

type(mono_type_t) :: source_type, target_type

! ✅ CORRECT: Use assignment operator (cycle-safe)
target_type = source_type

! ❌ INCORRECT: Manual field copying (unsafe for recursive types)
target_type%kind = source_type%kind  ! Incomplete, misses cycle safety
```

**AST Node Type Handling**: Assignment operators preserve inferred_type information:

```fortran
use ast_nodes_data, only: declaration_node

type(declaration_node) :: source_decl, target_decl

! ✅ CORRECT: Full assignment preserves semantic information
target_decl = source_decl  ! inferred_type safely copied

! Safe to access type information after assignment
if (allocated(target_decl%inferred_type)) then
    write(*,*) target_decl%inferred_type%to_string()
end if
```

**Performance Considerations**: For performance-critical code with simple types:

```fortran
! Simple types (TINT, TREAL, TCHAR, TLOGICAL) copy efficiently
integer_type = create_mono_type(TINT)
copy = integer_type  ! Fast, no recursion needed

! Complex function types use depth-limited copying
complex_fun_type = create_fun_type(arg_type, result_type)
copy = complex_fun_type  ! Cycle-safe, performance optimized
```