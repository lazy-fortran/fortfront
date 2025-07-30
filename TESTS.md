# Coverage Test Strategy

## Current Coverage Analysis

**Coverage Status**: 91.2% (31/34 measurable lines)
**Problem**: Only 34 lines across entire codebase are measurable for coverage
**Root Cause**: Most modules contain only interfaces/types, or functions not called by tests

## External API Usage (fortrun Integration)

**IMPORTANT**: Based on ../fortrun/FORTFRONT.md, fortrun extensively uses fortfront as a complete Fortran frontend for processing "lazy fortran" (.f/.F) files. The integration includes:

- **4-phase compilation pipeline**: lexer → parser → semantic analysis → code generation
- **Type inference**: Hindley-Milner algorithm W
- **Debug system**: 5 debug modes (tokens, ast, semantic, standardize, codegen)
- **Caching**: Content-based caching of preprocessed .f90 files
- **Main API entry points**: `compile_with_frontend()`, `compile_with_frontend_debug()`, `is_simple_fortran_file()`

This means most "zero coverage" modules are actually heavily used in production via fortrun.

## Complete Coverage Breakdown

### ✅ Fully Covered Modules (100%)
```
src/ast/ast_factory.f90                    1/1    lines
src/codegen/codegen_core.f90               1/1    lines  
src/codegen/codegen_indent.f90             1/1    lines
src/parser/parser_declarations.f90         1/1    lines
src/parser/parser_statements.f90           1/1    lines
src/semantic/semantic_analyzer_with_checks.f90  1/1 lines
src/semantic/semantic_checks.f90           1/1    lines
src/standardizer.f90                       1/1    lines
```

### ⚠️ Partially Covered Modules
```
src/semantic/type_system_hm.f90           23/25  lines (92%) - missing 699-700
src/json_reader.f90                       0/1    lines (0%)  - missing 709
```

### ❌ Zero Measurable Lines (BUT Used Heavily by fortrun API)
```
src/frontend.f90                          0      lines  ⭐ CORE API - 4-phase pipeline
src/lexer/lexer_core.f90                  0      lines  ⭐ CORE API - Phase 1 tokenization
src/parser/parser_core.f90                0      lines  ⭐ CORE API - Phase 2 AST construction
src/parser/parser_control_flow.f90        0      lines  ⭐ CORE API - if/do/select parsing
src/parser/parser_dispatcher.f90          0      lines  ⭐ CORE API - statement dispatcher
src/parser/parser_expressions.f90         0      lines  ⭐ CORE API - expression parsing
src/parser/parser_state.f90               0      lines  ⭐ CORE API - parser state mgmt
src/ast/ast_core.f90                      0      lines  ⭐ CORE API - AST arena management
src/json_writer.f90                       0      lines  ⭐ CORE API - debug output
src/semantic/parameter_tracker.f90        0      lines  ⭐ CORE API - parameter tracking
src/semantic/scope_manager.f90            0      lines  ⭐ CORE API - scope management
src/semantic/semantic_analyzer.f90        0      lines  ⭐ CORE API - Phase 3 type inference
src/semantic/type_checker.f90             0      lines  ⭐ CORE API - type compatibility
src/string_types.f90                      0      lines  ⭐ CORE API - string utilities
```

⭐ = Heavily used by fortrun's lazy fortran processing

## CLI Testing Strategy (NEW)

**Goal**: Test fortfront CLI interface for standalone usage and fortrun integration

**CLI Implementation**: fortfront now operates as a pure CLI tool:
- **Input**: Reads lazy fortran from stdin
- **Output**: Writes standard fortran to stdout
- **Errors**: Writes error messages to stderr with non-zero exit codes
- **Architecture**: Thin wrapper around existing 4-phase pipeline

### CLI Test Categories

#### String Transformation Unit Tests
**File**: `test/frontend/test_string_transformation.f90`
**Coverage**: Direct function calls to `transform_lazy_fortran_string()`
**Benefit**: Exercises entire pipeline (lexer→parser→semantic→codegen) with high coverage

```fortran
! Test 1: Hello world transformation
call transform_lazy_fortran_string("print *, 'Hello'", output, error)
assert(index(output, "program main") > 0)

! Test 2: Type inference
call transform_lazy_fortran_string("x = 42" // new_line('A') // "y = 3.14", output, error)  
assert(index(output, "integer :: x") > 0)
assert(index(output, "real") > 0)

! Test 3: Error handling
call transform_lazy_fortran_string("invalid syntax", output, error)
assert(len_trim(error) == 0)  ! Should handle gracefully
```

#### CLI System Tests  
**File**: `test/system/test_cli_integration.f90`
**Coverage**: CLI wrapper I/O logic (minimal - 2-3 tests only)
**Benefit**: Verify stdin/stdout interface works correctly

```fortran
! Test 1: Basic CLI I/O
command: echo "print *, 'test'" | fortfront
expected: Valid fortran output to stdout, exit code 0

! Test 2: Error handling  
command: echo "" | fortfront
expected: Minimal program output, exit code 0
```

#### Integration Tests (fortrun)
**File**: `../fortrun/test/integration/test_cli_integration.f90`
**Coverage**: End-to-end workflow with CLI subprocess
**Benefit**: Real-world validation of fortrun↔fortfront CLI integration

```fortran
! Test extension detection
assert(is_lazy_fortran_file("test.f"))
assert(.not. is_lazy_fortran_file("test.f90"))

! Test CLI subprocess execution
command = 'echo "x = 5" | fortfront > output.f90'
call execute_command_line(command, exitstat=exit_code)
assert(exit_code == 0)
```

### Updated Coverage Strategy

**Primary Coverage**: String transformation function calls (automatic pipeline coverage)
**Secondary Coverage**: CLI wrapper verification (minimal system tests)  
**Integration Coverage**: fortrun workflow testing (subprocess validation)

**Expected Results**:
- **Before**: 91.2% (31/34 lines)
- **After**: 95%+ coverage with ~100+ measurable lines
- **New measurable code**: String transformation (~30 lines), CLI wrapper (~20 lines)

## Outstanding Testing Opportunities

**Current Status**: 75 tests passing, good CLI and transformation coverage

**Remaining Gaps**: Focus on deep semantic analysis and error edge cases

### 1. Semantic Analysis Edge Cases (HIGH PRIORITY)

**Current Gap**: Type inference works but edge cases in Hindley-Milner algorithm untested

**Missing Tests**:
```fortran
! test/semantic/test_type_inference_edge_cases.f90
- Recursive function type inference: function calls itself with inferred types
- Polymorphic type unification failures: incompatible type constraints  
- Variable shadowing across scopes: same variable name different types
- Complex array type inference: multidimensional arrays with mixed expressions
- Function parameter type propagation: inferred types flowing through function chains
```

**Value**: Catches type system bugs that could generate invalid Fortran

### 2. Parser Error Recovery (MEDIUM PRIORITY)

**Current Gap**: Parser handles invalid syntax but recovery paths not systematically tested

**Missing Tests**:
```fortran
! test/parser/test_error_recovery.f90
- Incomplete control structures: missing end statements with nested blocks
- Malformed array syntax: unclosed brackets in complex expressions
- Invalid operator sequences: consecutive operators with precedence conflicts
- Partial function definitions: missing parameters or return types
- Mixed syntax styles: F77 vs F90 constructs in same program
```

**Value**: Ensures graceful degradation with malformed input

### 3. Code Generation Quality (MEDIUM PRIORITY)

**Current Gap**: Code generation works but output quality not validated

**Missing Tests**:
```fortran
! test/codegen/test_output_quality.f90
- Indentation consistency: nested blocks with correct spacing
- Variable declaration ordering: grouped by type with proper intent
- Standard compliance: generated code compiles with gfortran -std=f2008
- Optimization opportunities: redundant declarations removed
- Comment preservation: important comments maintained in output
```

**Value**: Ensures generated code is clean and maintainable

### 4. AST Transformation Correctness (LOW-MEDIUM PRIORITY)

**Current Gap**: AST operations work but semantic preservation not verified

**Missing Tests**:
```fortran
! test/ast/test_semantic_preservation.f90
- Type information preservation: types maintained through transformations
- Scope binding correctness: variable references point to correct declarations
- Expression evaluation order: operator precedence preserved in AST
- Control flow integrity: loop bounds and conditions unchanged
```

**Value**: Prevents semantic errors in transformed code

### 5. Memory Management Under Load (LOW PRIORITY)

**Current Gap**: AST arena works but stress testing missing

**Missing Tests**:
```fortran
! test/memory/test_arena_stress.f90
- Large AST handling: thousands of nodes without memory leaks
- Arena reuse patterns: multiple compilation cycles in same process
- Memory fragmentation: arena performance with mixed node sizes
```

**Value**: Ensures stability in production use

## Direct Function Coverage Strategy (URGENT)

**Problem**: Only 34 measurable lines detected by gcovr, most core modules show 0 lines
**Solution**: Create direct unit tests that call individual module functions

### Immediate High Priority Tests

#### 1. Lexer Core Direct Tests (`test/lexer/test_lexer_core_direct.f90`)
```fortran
use lexer_core, only: tokenize_source, classify_token, advance_position
! Test tokenize_source() with edge cases: empty strings, single chars, complex expressions
! Test classify_token() with boundary values: numbers, identifiers, operators
! Test position tracking: line numbers, column numbers across different inputs
```

#### 2. Parser Core Direct Tests (`test/parser/test_parser_core_direct.f90`)
```fortran
use parser_core, only: parse_expression, parse_statement, build_ast_node
! Test parse_expression() precedence: a+b*c, nested parentheses, mixed operators
! Test parse_statement() variants: assignments, control flow, declarations
! Test AST node construction: proper parent-child relationships, node types
```

#### 3. AST Core Direct Tests (`test/ast/test_ast_core_direct.f90`)
```fortran
use ast_core, only: create_node, destroy_node, traverse_ast, find_node_by_type
! Test node lifecycle: creation, modification, destruction
! Test tree traversal: pre-order, post-order, search algorithms
! Test arena memory management: allocation patterns, cleanup
```

#### 4. Semantic Analyzer Direct Tests (`test/semantic/test_semantic_direct.f90`)
```fortran
use semantic_analyzer, only: analyze_types, resolve_scope, infer_function_types
! Test type analysis: integer/real inference, array types, function return types
! Test scope resolution: nested scopes, variable shadowing, function parameters
! Test type inference: Hindley-Milner algorithm components with complex scenarios
```

#### 5. Type System Direct Tests (`test/semantic/test_type_system_direct.f90`)
```fortran
use type_system_hm, only: unify, substitute_type_vars, generalize, instantiate
! Test unification: type variable binding, constraint satisfaction, failure cases
! Test substitution: type variable replacement in complex expressions
! Test polymorphism: generic function instantiation, type scheme management
```

### Test Quality Requirements

- **Direct module imports**: `use module_name, only: function_name`
- **Controlled test data**: Small, specific inputs to exercise exact code paths
- **Boundary testing**: Empty inputs, single elements, maximum sizes
- **Fast execution**: Each test file under 5 seconds
- **Meaningful assertions**: Verify actual behavior, not just execution success

### Expected Coverage Improvement

- **Before**: 91.2% (31/34 lines) - most modules show 0 lines
- **After**: 95%+ coverage with 200+ measurable lines
- **New coverage**: Direct function calls will make gcovr detect actual code lines

### Implementation Order

1. **Lexer Core** - Foundation for all other phases
2. **AST Core** - Data structures used throughout  
3. **Parser Core** - AST construction logic
4. **Semantic Analyzer** - Type inference and checking
5. **Type System** - Hindley-Milner algorithm details

### Secondary Priority (Existing Strategy)

6. **Type Inference Edge Cases** - Complex semantic scenarios
7. **Parser Error Recovery** - Malformed input handling
8. **Code Generation Quality** - Output validation
9. **Memory Stress Testing** - Production stability

**Critical Success Metric**: gcovr should show 200+ measurable lines with 95%+ coverage after direct function tests are implemented.