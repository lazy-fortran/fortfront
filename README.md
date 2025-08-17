![fortfront](media/logo.svg)

Core analysis frontend for lazy fortran - transforms lazy Fortran to standard Fortran via CLI.

## Overview

fortfront is a pure CLI tool that transforms lazy Fortran code to standard Fortran:
- **Input**: Reads lazy fortran from stdin
- **Output**: Writes standard fortran to stdout
- **Pipeline**: 4-phase transformation (lexer → parser → semantic → codegen)
- **Type Inference**: Automatic variable type detection using Hindley-Milner algorithm
- **Integration**: Designed for use with fortrun build orchestrator (as fortfront)

## Features

- **Pure CLI Interface**: No API dependencies, works as standalone command
- **High Performance**: <0.05ms average transformation time
- **Robust Error Handling**: Graceful fallback for invalid syntax
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

### Integration with fortrun

fortfront is designed to work seamlessly with [fortrun](../fortrun):

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

## License

MIT License - see LICENSE file for details.

