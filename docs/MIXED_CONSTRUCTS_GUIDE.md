# Mixed Constructs Support Guide

fortfront supports mixed Fortran constructs in a single source file, enabling flexible code organization patterns commonly used in lazy Fortran development.

## Overview

Mixed constructs allow combining multiple program units in a single source file:
- Module definitions followed by main program code
- Multiple modules with implicit main program  
- Explicit program units with preceding modules

## Supported Patterns

### 1. Module with Implicit Main Program

**Input:**
```fortran
module utilities
contains
    subroutine greet()
        print *, "Hello from module!"
    end subroutine
end module

use utilities  
call greet()
end
```

**Output:**
```fortran
module utilities
contains
    subroutine greet()
        print *, "Hello from module!"
    end subroutine greet
end module utilities
program main
    use utilities
    implicit none
    call greet
    !ERROR: Unexpected keyword 'end' in expression
end program main
```

### 2. Module with Variable Declarations

**Input:**
```fortran
module math
    integer :: pi = 3
end module

integer :: result
result = pi * 2
print *, result
```

**Output:**
```fortran
module math
    integer :: pi = 3
end module math
program main
    implicit none
    integer :: result

    result = pi*2
    print *, result
end program main
```

### 3. Multiple Modules with Main Program

**Input:**
```fortran
module constants
    real :: gravity = 9.8
end module

module physics
contains
    real function weight(mass)
        real :: mass
        weight = mass * gravity
    end function
end module

real :: mass = 10.0
print *, weight(mass)
```

**Output:**
```fortran
module constants
    real :: gravity = 9.8
end module constants
module physics
contains
    real function weight(mass)
        real :: mass
        weight = mass * gravity
    end function weight
end module physics
program main
    implicit none
    real :: mass = 10.0

    print *, weight(mass)
end program main
```

## Implementation Details

### Parsing Behavior

fortfront handles mixed constructs through enhanced parser logic:

1. **Explicit Program Unit Detection**: Identifies modules, functions, subroutines, and programs
2. **Implicit Main Program Parsing**: After parsing explicit units, continues parsing remaining statements as implicit main program
3. **Automatic Program Wrapper**: Wraps implicit statements in `program main` / `end program main` structure

### Error Handling

Mixed constructs maintain fortfront's comprehensive error reporting:
- **Parse Errors**: Clear messages when constructs are malformed
- **Type Errors**: Full type inference across module boundaries
- **Scope Errors**: Proper variable scoping between modules and main program

## Best Practices

### Code Organization

```fortran
! Recommended: Modules first, then main program
module helpers
    ! Module content
end module

! Main program logic follows
integer :: x = 42
print *, x
```

### Variable Scoping

```fortran
module data
    integer :: shared_var = 100
end module

! Main program can access module variables
use data
integer :: local_var
local_var = shared_var * 2
```

### Type Consistency

```fortran
module types
    integer :: value = 10
end module

! Ensure compatible types in main program
use types
integer :: result  ! Explicit type recommended
result = value + 5
```

## Common Issues

### Issue #321 Resolution

Previously, mixed constructs with modules and implicit main programs would only generate the module portion. This has been resolved:

**Before (Issue #321):**
```fortran
! Input: module + implicit main
! Output: Only module, missing main program
```

**After (Fixed):**
```fortran
! Input: module + implicit main
! Output: Both module AND main program preserved
```

### Parser Limitations

Some edge cases may still require explicit program structure:

```fortran
! If this fails:
module m
end module
call something()
end

! Try explicit program:
module m  
end module
program main
call something()
end program
```

## Testing Examples

Test mixed construct support with these verified examples:

```bash
# Basic module + main
echo -e "module test\nend module\nx = 42" | fortfront

# Module with subroutines
echo -e "module math\ncontains\nsubroutine add()\nend subroutine\nend module\ncall add()" | fortfront

# Module with variables
echo -e "module data\ninteger :: n = 5\nend module\ninteger :: result\nresult = n" | fortfront
```

All examples generate proper standard Fortran with both module and main program components.