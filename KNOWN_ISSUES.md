# Known Issues

## Issue #176: Variable Initialization Values Removed During Formatting

**Status:** Partially Fixed / Known Limitation

**Description:** 
Variable initialization values are removed when processing Fortran code that contains both variable declarations with initializers and executable statements.

**Example:**
```fortran
! Input
program test
    implicit none
    integer :: x = 42
    real :: y = 3.14
    print *, x, y
end program test

! Output (incorrect)
program test
    implicit none
    integer :: x
    real(8) :: y
    
    print *, x, y
end program test
```

**Technical Details:**
The issue occurs during the standardization phase when the AST is processed. The standardizer correctly preserves the structure but the initializer indices are not properly maintained through the transformation pipeline when executable statements are present.

**Attempted Fixes:**
1. Modified `can_group_declarations` to prevent grouping of declarations with initializers
2. Updated `ast_factory` to properly check for non-zero initializer indices
3. Added fallback checks in code generation

**Workaround:**
For now, avoid using inline initialization in programs with executable statements. Instead, initialize variables separately:

```fortran
program test
    implicit none
    integer :: x
    real :: y
    
    x = 42
    y = 3.14
    print *, x, y
end program test
```

**Impact:**
This issue affects program semantics as initialized values are lost, potentially leading to undefined behavior if the variables are used before being assigned.

## Issue #177: Line Continuation Removal

**Status:** Architectural Limitation

**Description:**
Line continuations (`&`) are removed during parsing as the lexer handles them transparently. This is by design but means the original formatting with line continuations cannot be preserved.

**Workaround:**
The formatter will reformat long lines according to its own rules rather than preserving original line breaks.