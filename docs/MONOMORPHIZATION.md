# Type Inference and Monomorphization

## Current Behavior

Fortfront performs **single-file type inference** from call sites:

```fortran
! Input: lazy fortran (.lf)
function add(a, b)
    add = a + b
end function

x = add(5, 3)       ! Use with integers
y = add(2.5, 1.5)   ! Use with reals
```

**Output** (with monomorphization):
```fortran
module auto_add
    implicit none
    interface add
        module procedure add__i32_i32, add__r64_r64
    end interface add
contains
    integer function add__i32_i32(a, b)
        integer, intent(in) :: a, b
        add__i32_i32 = a + b
    end function

    real(8) function add__r64_r64(a, b)
        real(8), intent(in) :: a, b
        add__r64_r64 = a + b
    end function
end module auto_add

program main
    use auto_add
    implicit none
    integer :: x
    real(8) :: y
    x = add(5, 3)
    y = add(2.5, 1.5)
end program
```

## How It Works

1. **Parse** entire file into AST
2. **Collect all call sites** for each function
3. **Extract unique type signatures** from literal types
4. **For each unique signature**: Clone function body, substitute types, generate mangled name
5. **Create generic interface** binding all variants
6. **Emit module** with interface + specialized functions

## Naming Convention

Format: `<name>__<kind1>_<kind2>_...`

Examples:
- `add__i32_i32` - integer(4) + integer(4)
- `add__r64_r64` - real(8) + real(8)
- `matmul__i32rank2_i32rank2` - integer(4),dimension(:,:) matmul

## Scope and Limitations

**Fortfront handles**: Single-file monomorphization (complete solution)

**Package managers handle**: Cross-module specialization, caching, dependency resolution

**Limitations**:
- Single-file only - cannot infer across file boundaries
- Literal types only - cannot infer from variables with unknown types
- No explicit type constraints

## Cross-Module Specialization

Uses Fortran's generic interface extension mechanism. Library defines base generic, caller extends locally:

```fortran
! Library defines:
module m_add
  interface add
    module procedure add_int_int
  end interface add
end module

! Caller extends:
module m_caller_add_ext
  use m_add, only: add
  interface add
    module procedure add_real_real  ! New specific
  end interface add
end module
```

Fortran merges visible specifics at use site - no library mutation required.

## Implementation Status

Single-file monomorphization is complete and production-ready:
- Call graph tracks unique type signatures per function
- Codegen generates multiple specifics when needed
- Generic interface binds all specifics

Cross-module specialization (across file boundaries) is handled by package managers
using the Fortran generic interface extension mechanism described above.
