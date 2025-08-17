# Extensible Semantic Analysis Pipeline - User Guide

## Quick Start

```fortran
use semantic_pipeline, only: semantic_pipeline_t, create_pipeline
use builtin_analyzers, only: symbol_analyzer_t

type(semantic_pipeline_t) :: pipeline
type(symbol_analyzer_t) :: analyzer

pipeline = create_pipeline()
call pipeline%register_analyzer(analyzer)
call pipeline%run_analysis(arena, root_node_index)
```

## Overview

The extensible semantic analysis pipeline allows external tools to perform custom code analysis on Fortran ASTs. This system powers fortfront's semantic analysis and enables integration with external tools like **fluff** for static analysis.

## Core Concepts

### Analyzer Types

**Core Analyzers** - Essential for standardization:
- `symbol_analyzer_t` - Symbol collection and resolution
- `type_analyzer_t` - Hindley-Milner type inference
- `scope_analyzer_t` - Scope hierarchy management

**Analysis Plugins** - For external tools:
- `usage_tracker_analyzer_t` - Variable usage tracking
- `source_reconstruction_analyzer_t` - Source code mapping  
- `call_graph_analyzer_t` - Function call analysis
- `control_flow_analyzer_t` - Control flow analysis
- `interface_analyzer_t` - Interface consistency checking

### Pipeline Workflow

1. **Create Pipeline** - Initialize empty pipeline
2. **Register Analyzers** - Add analyzers for specific analysis needs
3. **Run Analysis** - Execute all registered analyzers on AST
4. **Extract Results** - Access analysis results for further processing

## Usage Patterns

### Basic Analysis

```fortran
use ast_core, only: ast_arena_t, create_ast_arena
use semantic_pipeline, only: semantic_pipeline_t, create_pipeline
use builtin_analyzers, only: symbol_analyzer_t

! Setup
type(semantic_pipeline_t) :: pipeline
type(symbol_analyzer_t) :: symbol_analyzer
type(ast_arena_t) :: arena
integer :: root_node_index

pipeline = create_pipeline()
arena = create_ast_arena()
root_node_index = 1

! Register analyzer
call pipeline%register_analyzer(symbol_analyzer)

! Execute analysis
call pipeline%run_analysis(arena, root_node_index)

! Check analyzer count
print *, "Executed", pipeline%get_analyzer_count(), "analyzers"
```

### External Tool Integration (Fluff Example)

```fortran
use semantic_pipeline, only: semantic_pipeline_t, create_pipeline
use builtin_analyzers, only: usage_tracker_analyzer_t, &
                             source_reconstruction_analyzer_t, &
                             call_graph_analyzer_t, &
                             control_flow_analyzer_t, &
                             interface_analyzer_t

! Create fluff-specific pipeline
type(semantic_pipeline_t) :: fluff_pipeline
type(usage_tracker_analyzer_t) :: usage_analyzer
type(source_reconstruction_analyzer_t) :: source_analyzer
type(call_graph_analyzer_t) :: call_graph_analyzer
type(control_flow_analyzer_t) :: control_flow_analyzer
type(interface_analyzer_t) :: interface_analyzer

fluff_pipeline = create_pipeline()

! Map fluff rules to analyzers
call fluff_pipeline%register_analyzer(usage_analyzer)      ! F006, F007 rules
call fluff_pipeline%register_analyzer(source_analyzer)     ! F014, F015 rules
call fluff_pipeline%register_analyzer(call_graph_analyzer) ! P001, P007 rules
call fluff_pipeline%register_analyzer(control_flow_analyzer) ! P002-P004 rules
call fluff_pipeline%register_analyzer(interface_analyzer)  ! F009 rules

! Run comprehensive analysis
call fluff_pipeline%run_analysis(arena, program_node)

print *, "Fluff analysis complete with", fluff_pipeline%get_analyzer_count(), "analyzers"
```

### Accessing Analysis Results

```fortran
! Access specific analyzer results
if (allocated(fluff_pipeline%analyzers(1)%analyzer)) then
    class(*), allocatable :: results
    results = fluff_pipeline%analyzers(1)%analyzer%get_results()
    
    ! Use results for rule checking
    select type(results)
    type is (usage_analysis_result_t)
        if (size(results%unused_variables) > 0) then
            print *, "F006 rule violations found"
        end if
    end select
end if
```

## Analyzer-Specific APIs

### Usage Tracker Analyzer

Detects unused and undefined variables (F006, F007 rules):

```fortran
type(usage_tracker_analyzer_t) :: usage_analyzer

! After analysis, use specialized methods
character(:), allocatable :: unused_vars(:)
character(:), allocatable :: undefined_vars(:)

unused_vars = usage_analyzer%find_unused_variables()
undefined_vars = usage_analyzer%find_undefined_variables()

! Check specific variable usage
logical :: is_used
is_used = usage_analyzer%is_variable_used("variable_name")

! Get usage locations
integer, allocatable :: locations(:)
locations = usage_analyzer%get_usage_locations("variable_name")
```

### Source Reconstruction Analyzer

Provides source code mapping (F014, F015 rules):

```fortran
type(source_reconstruction_analyzer_t) :: source_analyzer

! Extract source text for nodes
character(:), allocatable :: source_text
source_text = source_analyzer%get_node_source_text(node_index)

! Get specific line text
character(:), allocatable :: line_text
line_text = source_analyzer%get_line_text(line_number)

! Extract text spans
character(:), allocatable :: text_span
text_span = source_analyzer%extract_text_span(start_line, start_col, end_line, end_col)

! Get context around nodes
character(:), allocatable :: context
context = source_analyzer%get_context_around_node(node_index, context_lines=3)

! Format location strings
character(:), allocatable :: location_str
location_str = source_analyzer%format_source_location(node_index)
```

## Complete Integration Example

```fortran
program semantic_analysis_demo
    use ast_core, only: ast_arena_t, create_ast_arena
    use semantic_pipeline, only: semantic_pipeline_t, create_pipeline
    use builtin_analyzers, only: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t, &
                                 usage_tracker_analyzer_t, source_reconstruction_analyzer_t
    use frontend, only: lex_source, parse_tokens
    use lexer_core, only: token_t
    implicit none

    ! Sample Fortran code with potential issues
    character(len=*), parameter :: fortran_code = &
        "program test" // new_line('a') // &
        "  implicit none" // new_line('a') // &
        "  integer :: used_var, unused_var" // new_line('a') // &
        "  used_var = 42" // new_line('a') // &
        "  print *, used_var" // new_line('a') // &
        "end program test"

    ! Pipeline components
    type(semantic_pipeline_t) :: comprehensive_pipeline
    type(symbol_analyzer_t) :: symbol_analyzer
    type(type_analyzer_t) :: type_analyzer
    type(scope_analyzer_t) :: scope_analyzer
    type(usage_tracker_analyzer_t) :: usage_analyzer
    type(source_reconstruction_analyzer_t) :: source_analyzer
    
    ! AST components
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    integer :: prog_index
    character(len=:), allocatable :: error_msg

    print *, "=== Comprehensive Semantic Analysis Demo ==="

    ! Step 1: Parse Fortran code
    call lex_source(fortran_code, tokens, error_msg)
    if (len_trim(error_msg) > 0) error stop "Lexing failed"
    
    call parse_tokens(tokens, arena, prog_index, error_msg)
    if (len_trim(error_msg) > 0) error stop "Parsing failed"

    print *, "✓ Fortran code parsed successfully"

    ! Step 2: Set up comprehensive analysis pipeline
    comprehensive_pipeline = create_pipeline()
    call comprehensive_pipeline%register_analyzer(symbol_analyzer)
    call comprehensive_pipeline%register_analyzer(type_analyzer)
    call comprehensive_pipeline%register_analyzer(scope_analyzer)
    call comprehensive_pipeline%register_analyzer(usage_analyzer)
    call comprehensive_pipeline%register_analyzer(source_analyzer)

    print *, "✓ Registered", comprehensive_pipeline%get_analyzer_count(), "analyzers"

    ! Step 3: Run comprehensive analysis
    call comprehensive_pipeline%run_analysis(arena, prog_index)
    print *, "✓ Analysis completed successfully"

    ! Step 4: Extract and process results
    if (allocated(comprehensive_pipeline%analyzers(4)%analyzer)) then
        ! Check for unused variables (F006 rule)
        character(:), allocatable :: unused_vars(:)
        unused_vars = comprehensive_pipeline%analyzers(4)%analyzer%find_unused_variables()
        
        if (size(unused_vars) > 0) then
            print *, "⚠ F006 rule violations (unused variables):"
            block
                integer :: i
                do i = 1, size(unused_vars)
                    print *, "  - ", unused_vars(i)
                end do
            end block
        else
            print *, "✓ No unused variables found"
        end if
    end if

    print *, "=== Analysis Complete ==="

end program semantic_analysis_demo
```

## Performance Guidelines

### Memory Management
- Pipelines automatically clean up analyzers
- Reuse pipelines for multiple files to improve performance
- Use `call pipeline%clear_analyzers()` to reset if needed

### Scalability
- Pipeline handles large codebases efficiently
- Each analyzer processes AST independently
- Memory usage scales linearly with AST size

### Error Resilience
- Pipeline handles invalid node indices gracefully
- Empty AST arenas are processed safely
- Multiple registrations of same analyzer type are supported

## Best Practices

### For External Tool Developers

1. **Selective Analyzer Registration** - Only register analyzers needed for your rules
2. **Result Type Checking** - Always use `select type` when accessing results
3. **Error Handling** - Check `allocated` status before accessing analyzers
4. **Pipeline Reuse** - Create pipeline once, reuse for multiple files

### For Analysis Plugin Developers

1. **Memory Safety** - Always deallocate arrays before reallocating
2. **Deep Copy** - Implement assignment operators for proper copying
3. **Result Types** - Define clear result types with meaningful data
4. **Method Names** - Use descriptive names that indicate analysis type

## Integration Checklist

- [ ] Import required modules (`semantic_pipeline`, `builtin_analyzers`)
- [ ] Create pipeline with `create_pipeline()`
- [ ] Register appropriate analyzers for your use case
- [ ] Parse Fortran code into AST arena
- [ ] Run analysis with `call pipeline%run_analysis(arena, root_node)`
- [ ] Extract results using analyzer-specific methods
- [ ] Handle empty results and error cases gracefully
- [ ] Clean up resources (automatic with pipeline finalization)

## Troubleshooting

### Common Issues

**"Unknown analyzer type" error**
- Ensure analyzer type is imported and supported in `register_analyzer`
- Check that analyzer extends `semantic_analyzer_t`

**Empty results**
- Verify AST arena contains valid nodes
- Check that analysis completed without errors
- Ensure correct node index is passed to `run_analysis`

**Memory allocation errors**
- Clear previous results before new analysis
- Avoid manual deallocation of pipeline-managed resources
- Use proper assignment operators for result copying

### Performance Issues

**Slow analysis on large files**
- Profile individual analyzers to identify bottlenecks
- Consider reducing context_lines for source reconstruction
- Use selective analyzer registration

**High memory usage**
- Clear intermediate results when not needed
- Reuse pipelines instead of creating new ones
- Monitor AST arena size growth