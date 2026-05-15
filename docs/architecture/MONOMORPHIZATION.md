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

    real(dp) function add__r64_r64(a, b)
        real(dp), intent(in) :: a, b
        add__r64_r64 = a + b
    end function
end module auto_add

program main
    use auto_add
    implicit none
    integer :: x
    real(dp) :: y
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
- `add__r64_r64` - real(dp) + real(dp)
- `matmul__i32rank2_i32rank2` - integer(4),dimension(:,:) matmul

## Scope and Limitations

**Fortfront handles**: Single-file monomorphization experiments for Lazy
Fortran transformation.

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

Single-file monomorphization exists in the transformation pipeline, but it
should not be treated as a finished compiler feature. It is useful for examples
and local Lazy Fortran standardization; it is not a complete generic system.

Before compiler backends depend on it, the following must be made explicit:

- Which call-site signatures are collected and which are ignored.
- How diagnostics are reported when specialization fails.
- How generated specifics interact with module boundaries.
- Which behavior intentionally differs from LFortran Infer mode.

Cross-module specialization remains outside FortFront. Package/build tooling or
a compiler driver must own dependency order, caching, and cross-file decisions.
