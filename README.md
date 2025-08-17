![fortfront](media/logo.svg)

Core analysis frontend for lazy fortran - transforms lazy Fortran to standard Fortran via CLI.

## Overview

fortfront is a pure CLI tool that transforms lazy Fortran code to standard Fortran:
- **Input**: Reads lazy fortran from stdin
- **Output**: Writes standard fortran to stdout
- **Pipeline**: 4-phase transformation (lexer → parser → semantic → codegen)
- **Type Inference**: Automatic variable type detection using Hindley-Milner algorithm
- **Integration**: Designed for use with fortrun build orchestrator (as fortfront)

## Array Literal Support

fortfront supports comprehensive array literal parsing for all Fortran data types:

### ✅ Supported Array Types

**Numeric arrays:**
```bash
echo "arr = [1, 2, 3]" | fortfront           # Integer arrays
echo "arr = [1.0, 2.5, 3.14]" | fortfront    # Real arrays
echo "arr = []" | fortfront                   # Empty arrays
```

**Logical arrays:**
```bash
echo "arr = [.true., .false.]" | fortfront    # Logical literal arrays
echo "flag = .true." | fortfront              # Individual logical literals
```

**Variable and expression arrays:**
```bash
echo "arr = [a, b, c]" | fortfront            # Variable arrays
echo "arr = [x + y, z * 2]" | fortfront       # Expression arrays
echo "arr = [func(1), func(2)]" | fortfront   # Function call arrays
```

**Mixed type arrays:**
```bash
echo "arr = [1, 2.0, 3]" | fortfront          # Mixed numeric types
```

### Recent Improvements

**Issue #261 Resolution**: Parser regressions affecting logical literal arrays have been resolved. Array literal parsing now works correctly for all types including logical literals (`[.true., .false.]`) while maintaining the enhanced error reporting improvements from Issue #256.

## Features

- **Pure CLI Interface**: No API dependencies, works as standalone command
- **High Performance**: <0.05ms average transformation time
- **Enhanced Error Reporting**: Clear error messages with line/column info and fix suggestions
- **Comprehensive Testing**: Unit tests for transformation function + CLI system tests
- **Standard Compliant**: Generates clean, standard Fortran code
- **Type Inference**: Hindley-Milner algorithm for automatic variable typing
- **Extensible Semantic Analysis**: Plugin-based semantic analysis pipeline for external tools

## Building

```bash
fpm build
```

## Testing

```bash
fpm test
```

## Usage

### Command Line Interface

Transform lazy Fortran to standard Fortran:

```bash
# Basic usage - pipe lazy fortran through fortfront
echo "x = 42" | fortfront

# With file input/output
fortfront < input.lf > output.f90

# Example transformation
echo -e "x = 42\ny = 3.14\nprint *, x + y" | fortfront
```

**Output:**
```fortran
program main
    implicit none
    integer :: x
    real(8) :: y

    x = 42
    y = 3.14d0
    print *, x + y
end program main
```

### Input Validation

fortfront uses a dedicated `input_validation` module to validate input and provide enhanced error reporting before attempting transformation. This modular design separates validation concerns from the main frontend processing pipeline.

#### ✅ Accepted Input Types

**Comments and empty input:**
```bash
echo "! This is a comment" | fortfront
echo "" | fortfront  # Empty input generates minimal program
```

**Variable assignments:**
```bash
echo "x = 42" | fortfront
echo "result = 3.14 * radius" | fortfront
```

**Mathematical expressions:**
```bash
echo "a + b" | fortfront          # Simple operations
echo "(x + y) * z" | fortfront    # Expressions with parentheses
echo "x * y + z / 2.0" | fortfront # Mixed identifier/number expressions
```

**Function calls:**
```bash
echo "sqrt(x) + y" | fortfront
echo "result = sin(angle)" | fortfront
```

**Array operations:**
```bash
echo "arr(i)" | fortfront
echo "matrix(i, j) = value" | fortfront
```

#### ❌ Rejected Input Types

Input without clear Fortran structure is rejected with helpful error messages:

```bash
echo "random text" | fortfront
# Error: Input does not appear to be valid Fortran code. No recognized Fortran patterns found.

echo "123 ++ 456" | fortfront  
# Error: Invalid syntax patterns detected.
```

#### Validation Architecture

Input validation is handled by the dedicated `input_validation` module, which provides:

**Modular Design:**
- Clean separation from frontend processing logic
- Reusable validation functions for different use cases
- Independent error reporting with enhanced diagnostics
- No circular dependencies with other modules

**Core Validation Functions:**
- `validate_basic_syntax()` - Main validation entry point
- `check_missing_then_statements()` - Detects if-without-then errors
- `check_incomplete_statements()` - Finds dangling operators and incomplete expressions
- `check_for_fortran_content()` - Validates input contains recognizable Fortran patterns
- `check_missing_end_constructs()` - Detects missing end statements
- `contains_invalid_patterns()` - Identifies invalid syntax patterns
- `has_only_meaningless_tokens()` - Filters out non-Fortran content

**Multi-Phase Validation Process:**
1. **Phase 1:** Accept comments-only or empty input
2. **Phase 2:** Accept input containing Fortran keywords (implicit, program, etc.)
3. **Phase 3:** Check for likely Fortran patterns:
   - Must contain identifiers
   - Must have assignments, function calls, OR mathematical operators with operands
   - Limited unknown tokens allowed
4. **Phase 4:** Reject input without recognizable structure

This modular approach ensures lazy Fortran expressions are properly accepted while preventing meaningless input from causing confusing transformation failures.

#### Mathematical Expression Validation

**Fixed (Issue #259):** The validation logic now correctly accepts mathematical expressions with operators, whether they contain numbers or only identifiers:

```bash
# All mathematical expressions now work correctly:
echo "a + b" | fortfront          # ✅ Works: identifier-only expressions accepted
echo "x * y" | fortfront          # ✅ Works: identifier-only expressions accepted
echo "(a + b) * c" | fortfront    # ✅ Works: complex identifier expressions accepted
echo "x + 1" | fortfront          # ✅ Works: expressions with numbers accepted
echo "a * 2.0" | fortfront        # ✅ Works: expressions with numbers accepted
```

**Implementation:** The validation functions were extracted from `src/frontend.f90` into the dedicated `src/input_validation.f90` module. The main validation logic now accepts mathematical expressions based on the presence of operators alone, rather than requiring both operators and numbers. This supports lazy Fortran's goal of allowing concise mathematical expressions with identifiers only.

#### Standalone Validation Usage

The `input_validation` module can be used independently of the main fortfront frontend:

```fortran
use lexer_core, only: token_t, lex_source
use input_validation, only: validate_basic_syntax

character(len=:), allocatable :: source, error_msg
type(token_t), allocatable :: tokens(:)

! Example: Validate Fortran code without transformation
source = "if x > 0" // new_line('A') // "  print *, x"
call lex_source(source, tokens, error_msg)
call validate_basic_syntax(source, tokens, error_msg)

if (error_msg /= "") then
    print *, "Validation error: ", error_msg
    ! Error: Missing 'then' statement at line 1, column 9
    ! Source: if x > 0
    !         --------^
    ! Suggestion: Add 'then' after the condition
else
    print *, "Input is valid Fortran"
end if
```

**Use Cases for Standalone Validation:**
- **Editor Integration**: Real-time syntax checking in code editors
- **Build Tools**: Pre-compilation validation in build systems
- **Code Quality Tools**: Lint-style checking for Fortran code
- **Educational Tools**: Teaching Fortran syntax with immediate feedback

**Validation API:**
```fortran
! Primary validation interface
call validate_basic_syntax(source, tokens, error_msg)

! Specific validation checks (can be used individually)
call check_missing_then_statements(tokens, source_lines, error_msg)
call check_incomplete_statements(tokens, source_lines, error_msg)
call check_for_fortran_content(tokens, error_msg)
call check_missing_end_constructs(tokens, source_lines, error_msg)

! Utility functions
logical :: has_invalid_patterns = contains_invalid_patterns(tokens)
logical :: meaningless_input = has_only_meaningless_tokens(tokens)
```

### Integration with fortrun

fortfront is designed to work seamlessly with [fortrun](https://github.com/lazy-fortran/fortrun):

```bash
# fortrun automatically uses fortfront for .lf files
fortrun hello.lf

# Explicit usage in build scripts
fortfront < lazy_source.lf > standard_source.f90
gfortran standard_source.f90 -o program
```

### Library Usage (Advanced)

For direct integration in Fortran applications:

```fortran
use frontend, only: transform_lazy_fortran_string

character(len=:), allocatable :: input, output, error_msg

input = "x = 42" // new_line('A') // "print *, x"
call transform_lazy_fortran_string(input, output, error_msg)

if (error_msg == "") then
    print *, output
else
    print *, "Error: ", error_msg
end if
```

### Extensible Semantic Analysis

fortfront provides a plugin-based semantic analysis pipeline that external tools can use to perform custom code analysis. This is particularly useful for static analysis tools like **fluff**.

#### Basic Pipeline Usage

```fortran
use ast_core, only: ast_arena_t, create_ast_arena
use semantic_pipeline, only: semantic_pipeline_t, create_pipeline
use builtin_analyzers, only: symbol_analyzer_t

! Create pipeline
type(semantic_pipeline_t) :: pipeline
type(symbol_analyzer_t) :: symbol_analyzer
type(ast_arena_t) :: arena

pipeline = create_pipeline()
arena = create_ast_arena()

! Register analyzers
call pipeline%register_analyzer(symbol_analyzer)

! Run analysis
call pipeline%run_analysis(arena, root_node_index)

! Access results
class(*), allocatable :: results
results = pipeline%analyzers(1)%analyzer%get_results()
```

#### Built-in Analyzers

**Core Analyzers** (for standardization):
- `symbol_analyzer_t` - Symbol collection and resolution
- `type_analyzer_t` - Hindley-Milner type inference  
- `scope_analyzer_t` - Scope hierarchy management

**Analysis Plugins** (for external tools):
- `usage_tracker_analyzer_t` - Variable usage analysis (F006, F007 rules)
- `source_reconstruction_analyzer_t` - Source mapping (F014, F015 rules)
- `call_graph_analyzer_t` - Function call analysis (P001, P007 rules)
- `control_flow_analyzer_t` - Control flow analysis (P001-P004 rules)
- `interface_analyzer_t` - Interface consistency (F009 rules)

#### Fluff Integration Example

```fortran
use semantic_pipeline, only: semantic_pipeline_t, create_pipeline
use builtin_analyzers, only: usage_tracker_analyzer_t, &
                             source_reconstruction_analyzer_t

! Set up pipeline for fluff rules
type(semantic_pipeline_t) :: fluff_pipeline
type(usage_tracker_analyzer_t) :: usage_analyzer
type(source_reconstruction_analyzer_t) :: source_analyzer

fluff_pipeline = create_pipeline()

! Register analyzers for specific rule categories
call fluff_pipeline%register_analyzer(usage_analyzer)      ! F006, F007
call fluff_pipeline%register_analyzer(source_analyzer)     ! F014, F015

! Run analysis on user's code
call fluff_pipeline%run_analysis(arena, program_node)

! Extract results for rule checking
if (allocated(fluff_pipeline%analyzers(1)%analyzer)) then
    unused_vars = fluff_pipeline%analyzers(1)%analyzer%find_unused_variables()
    ! Apply F006 rule checks...
end if
```

#### Multiple Analyzer Execution

```fortran
use builtin_analyzers, only: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t, &
                             call_graph_analyzer_t, control_flow_analyzer_t

! Comprehensive analysis pipeline
pipeline = create_pipeline()
call pipeline%register_analyzer(symbol_analyzer_t())
call pipeline%register_analyzer(type_analyzer_t())
call pipeline%register_analyzer(scope_analyzer_t())
call pipeline%register_analyzer(call_graph_analyzer_t())
call pipeline%register_analyzer(control_flow_analyzer_t())

! Run all analyzers
call pipeline%run_analysis(arena, root_node)

print *, "Executed", pipeline%get_analyzer_count(), "analyzers"
```

#### Performance Considerations

The semantic pipeline is designed for production use:
- **Memory Safe**: Automatic cleanup, no memory leaks
- **Scalable**: Handles large codebases efficiently  
- **Reusable**: Pipelines can be reused for multiple files
- **Type Safe**: Compile-time type checking for all analyzers

#### Error Handling

```fortran
! Pipeline handles edge cases gracefully
call pipeline%run_analysis(empty_arena, invalid_node)  ! Safe
call pipeline%run_analysis(arena, -1)                  ! Safe
call pipeline%run_analysis(arena, 999999)              ! Safe

! Empty pipeline execution is safe
empty_pipeline = create_pipeline()
call empty_pipeline%run_analysis(arena, node)          ! No-op, safe
```

#### Error Handling in Library Usage

The `transform_lazy_fortran_string` subroutine performs input validation and returns appropriate error messages:

```fortran
! Valid mathematical expression (now works with identifiers only)
input = "a + b * c"
call transform_lazy_fortran_string(input, output, error_msg)
! error_msg will be empty, output contains transformed code

! Invalid input 
input = "random text without fortran structure"
call transform_lazy_fortran_string(input, output, error_msg)
! error_msg = "Input does not appear to be valid Fortran code. No recognized Fortran patterns found."

! Comments-only input (valid)
input = "! This is just a comment"
call transform_lazy_fortran_string(input, output, error_msg)
! error_msg is empty, output contains minimal valid program
```

## Error Reporting

fortfront provides comprehensive error reporting with detailed information to help fix syntax issues:

### Error Message Features

- **Precise Location**: Line and column numbers for each error
- **Clear Descriptions**: Specific problem identification (e.g., "Missing 'then' keyword")
- **Fix Suggestions**: Actionable advice when possible (e.g., "Add 'then' after condition")
- **Source Context**: Shows the problematic source line with position indicators

### Example Error Output

```bash
echo "if x > 0" | fortfront
```

**Error Output:**
```
Missing 'then' keyword in if statement at line 1, column 1
  Source: if x > 0
           ^
  Suggestion: Add 'then' after the if condition
```

### Handling Invalid Input

- **No Silent Failures**: All syntax errors are explicitly reported
- **Meaningful Fallback**: Invalid syntax produces error comments in output rather than empty programs
- **Comprehensive Validation**: Comments-only files and simple expressions are properly accepted

## License

MIT License - see LICENSE file for details.

