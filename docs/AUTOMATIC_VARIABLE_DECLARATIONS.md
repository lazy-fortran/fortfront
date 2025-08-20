# Automatic Variable Declaration Generation

## Overview

fortfront automatically generates variable declarations for functions when types can be inferred from usage. This eliminates the need for explicit type declarations in simple cases while maintaining standard Fortran compliance.

## How It Works

### Successful Type Inference

When variables are used with sufficient type context, fortfront generates appropriate declarations:

```bash
echo "function twice(x) result(y)
    y = 2.0 * x
end function twice" | fortfront
```

**Output:**
```fortran
function twice(x) result(y)
    implicit none
    real :: x
    real :: y
    y = 2.0d0*x
end function twice
```

### Type Inference Rules

fortfront can infer types from:

- **Literals**: `x = 5.0` → `real :: x`
- **String literals**: `name = "hello"` → `character(len=5) :: name`
- **Logical values**: `flag = .true.` → `logical :: flag`
- **Integer literals**: `count = 42` → `integer :: count`

### Error Cases

fortfront emits errors when types cannot be determined:

```bash
echo "function unclear(x) result(y)
    y = 2 * x
end function unclear" | fortfront
```

**Error:**
```
Cannot infer type for variable 'x' in function 'unclear'
Add explicit type declaration for this variable
```

## Function Scope Only

Automatic declaration applies only to functions. Program-level variables retain implicit typing behavior:

```bash
# Program level - no automatic declarations
echo "x = 42" | fortfront
```

```bash
# Function level - automatic declarations generated
echo "function calc() result(x)
    x = 42
end function calc" | fortfront
```

## Error Examples

### Insufficient Context

Variables without type evidence require explicit declarations:

```fortran
! Error: Cannot infer type
function ambiguous(a, b) result(c)
    c = a + b  ! No literals to infer from
end function ambiguous
```

### Solution: Add explicit declarations

```fortran
! Working: Explicit types provided
function explicit(a, b) result(c)
    real :: a, b, c
    c = a + b
end function explicit
```

## Integration

This feature works transparently within fortfront's compilation pipeline. No configuration required.