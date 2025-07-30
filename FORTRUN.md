# Fortfront-Fortrun Decoupling Plan

## Overview

This document outlines the plan to create a clean, well-defined API interface between fortfront and fortrun. fortfront will focus solely on the frontend transformation (lazy fortran → standard fortran), while fortrun handles compilation, caching, and project management.

## Current Coupling Analysis

### Direct Dependencies (fortrun → fortfront)
1. **`frontend_integration.f90`** - Current API layer (incomplete)
2. **`debug_state.f90`** - Global debug flag management
3. **File type detection** - `is_simple_fortran_file()` function
4. **Compilation functions** - `compile_with_frontend()`, `compile_with_frontend_debug()`

### Tight Coupling Issues
1. **Mixed responsibilities** - fortfront handles compilation, should only handle transformation
2. **Debug flag management** - Split between fortrun CLI and fortfront debug_state
3. **Error handling** - Inconsistent error propagation
4. **File type detection** - Basic implementation needs enhancement
5. **API surface** - Multiple modules instead of single clean interface
6. **No CLI interface** - No standalone fortfront CLI for transformation

## Proposed Clean Design

### Pure CLI Approach - No API Needed!

Since fortrun's `scan_modules` works on any standard Fortran file (it just parses `use` statements), we don't need any API calls between fortrun and fortfront.

**fortfront** becomes a pure Unix tool:
- Simple CLI: `fortfront` (reads stdin, writes stdout)
- No API, no library interface
- Just like `cat`, `grep`, `sed` - pure transformation tool

**fortrun** just calls it as a subprocess:
```fortran
! In fortrun - no imports from fortfront needed!
if (is_lazy_fortran_file(filename)) then  ! Simple .f/.F extension check
    ! Transform via CLI subprocess
    command = 'fortfront < '//trim(input_file)//' > '//trim(output_file)
    call execute_command_line(command, exitstat=status)
    
    if (status /= 0) then
        call print_error('Fortfront transformation failed')
        return
    end if
    
    ! Now scan the generated .f90 file for dependencies (same as any .f90)
    call scan_modules(output_file, modules, n_modules)
end if
```

## Implementation Plan

### Minimal `fortfront` CLI

```bash
# Transform lazy fortran via stdin/stdout (ONLY operation)
cat hello.f | fortfront > hello.f90

# That's it! Pure Unix philosophy: one tool, one job
cat calculate.f | fortfront > calculate.f90

# File to file (if needed)
fortfront < input.f > output.f90
```

**Implementation**: 
- Minimal `app/main.f90` - just read stdin, transform, write stdout
- No arguments, no options, no complexity
- Error messages go to stderr
- Exit code 0 for success, 1 for errors

### Code Changes Needed

#### In fortfront:
1. **Create `app/main.f90`** - Minimal CLI that reads stdin, transforms, writes stdout
2. **Remove API complexity** - No need for `fortfront.f90` module, options types, etc.
3. **Keep transformation pipeline** - The 4-phase pipeline (lexer→parser→semantic→codegen) stays

#### In fortrun:
1. **Remove fortfront imports** - No more `use frontend_integration` or `use debug_state`
2. **Add simple file extension check** - Basic `.f/.F` detection in fortrun itself
3. **Use subprocess calls** - `execute_command_line()` to call `fortfront` CLI
4. **Keep existing logic** - Caching, `scan_modules`, compilation, etc. all stay the same

## Benefits of Pure CLI Approach

### 1. Ultimate Simplicity
- **fortfront**: Pure Unix tool, no API surface, no complexity
- **fortrun**: No dependencies on fortfront internals, just subprocess calls
- **Decoupling**: Complete separation - they communicate via files and exit codes

### 2. Better Testing  
- **fortfront**: `echo "code" | fortfront` for instant testing
- **fortrun**: Can test with any transformation tool that follows same interface
- **Integration**: Test the interface, not the implementation

### 3. Tool Interoperability
- Any tool can use `fortfront` (editors, IDEs, other build systems)
- No need to link against fortfront libraries
- Standard Unix pipes and redirects work

### 4. Maintenance
- **fortfront**: Focus only on transformation quality
- **fortrun**: Focus only on build orchestration
- **Independence**: Changes in one don't affect the other (as long as CLI contract holds)

## Migration Strategy - Pure CLI Approach

### Phase 1: Create Minimal fortfront CLI
1. **Create `app/main.f90`** - Read stdin, call existing pipeline, write stdout  
2. **Add to fpm.toml** - Configure executable
3. **Remove API complexity** - No need for `fortfront.f90` module, options types
4. **Test CLI directly** - Unit tests via `echo "code" | fortfront`

### Phase 2: Update fortrun Integration  
1. **Add file extension check** - Simple `.f/.F` detection in fortrun
2. **Replace API calls** - Use `execute_command_line()` to call `fortfront` CLI
3. **Remove fortfront imports** - Delete `use frontend_integration`, `use debug_state`
4. **Test integration** - End-to-end tests with subprocess calls

### Phase 3: Cleanup and Optimization
1. **Remove unused modules** - Delete `frontend_integration.f90`, `debug_state.f90`
2. **Update documentation** - Reflect pure CLI approach
3. **Performance testing** - Ensure no regression from subprocess overhead
4. **Error handling** - Clean error messages from CLI failures

## Implementation Details

### fortfront CLI Implementation
```fortran
! app/main.f90 - Minimal implementation
program fortfront_cli
    use frontend, only: compile_source, compilation_options_t
    implicit none
    
    character(len=:), allocatable :: input_code, output_code
    type(compilation_options_t) :: options
    character(len=256) :: error_msg
    integer :: input_unit, stat
    
    ! Read all from stdin
    call read_stdin(input_code)
    
    ! Transform using existing pipeline
    call compile_source_from_string(input_code, options, output_code, error_msg)
    
    if (len_trim(error_msg) > 0) then
        write(*, '(A)') trim(error_msg)  ! Error to stderr
        stop 1
    end if
    
    ! Write to stdout
    write(*, '(A)') output_code
    stop 0
    
contains
    subroutine read_stdin(content)
        ! Implementation to read all stdin into string
    end subroutine
end program
```

### fortrun Integration Changes
```fortran
! In fortrun/src/runner/runner.f90 - Remove all fortfront imports
! Add simple file extension check
logical function is_lazy_fortran_file(filename)
    character(len=*), intent(in) :: filename
    integer :: ext_pos
    
    ext_pos = index(filename, '.', back=.true.)
    if (ext_pos > 0) then
        is_lazy_fortran_file = filename(ext_pos:) == '.f' .or. filename(ext_pos:) == '.F'
    else
        is_lazy_fortran_file = .false.
    end if
end function

! Replace transformation calls
if (is_lazy_fortran_file(filename)) then
    command = 'fortfront < '//trim(input_file)//' > '//trim(output_file)//' 2>error.log'
    call execute_command_line(command, exitstat=status)
    
    if (status /= 0) then
        ! Read error.log and report
        call handle_transformation_error()
        return
    end if
end if
```

## Success Metrics

1. **Complete decoupling** - fortrun has zero imports from fortfront
2. **CLI functionality** - `fortfront` works standalone for any tool
3. **No performance regression** - Subprocess overhead negligible  
4. **Test coverage** - All transformation paths covered via CLI tests
5. **Tool interoperability** - Other tools can easily use `fortfront` CLI

## Benefits Achieved

1. **Ultimate simplicity** - Two independent tools with clean interface
2. **Unix philosophy** - One tool, one job, composable
3. **Zero coupling** - Complete architectural independence  
4. **Universal usability** - Any tool can use fortfront without linking