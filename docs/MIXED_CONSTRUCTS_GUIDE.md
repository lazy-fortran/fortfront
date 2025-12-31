# Mixed Constructs Support

fortfront supports combining multiple program units in a single source file.

## Module with Implicit Main Program

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
end program main
```

## Multiple Modules with Main Program

**Input:**
```fortran
module constants
    real :: gravity = 9.8
end module

module physics
    use constants
contains
    real function weight(mass)
        real :: mass
        weight = mass * gravity
    end function
end module

real :: mass = 10.0
print *, weight(mass)
```

**Output:** Both modules preserved, followed by `program main` wrapping the remaining statements.

## Parsing Behavior

1. **Explicit units detected**: modules, functions, subroutines, programs
2. **Implicit main parsed**: remaining statements after explicit units
3. **Automatic wrapper**: implicit statements wrapped in `program main` / `end program main`

## Issue #321 Resolution

Previously, mixed constructs only generated the module portion. This has been fixed - both module AND main program are now preserved.

## Edge Cases

If implicit main fails to parse, try explicit program structure:
```fortran
module m
end module
program main
    call something()
end program
```
